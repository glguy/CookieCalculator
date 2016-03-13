module Main (main) where

import CookieClicker
import GameInput
import qualified Control.Lens as L
import Data.Time
import Data.List
import Data.Ord
import SaveFormat
import Data.Foldable
import Graphics.UI.Gtk

myWindow =
  do window <- windowNew
     on window objectDestroy mainQuit
     set window [ containerBorderWidth := 10, windowTitle := "Cookie Calculator" ]
     return window

myColumn grid name =
  do col <- treeViewColumnNew
     set col [ treeViewColumnTitle := name ]
     treeViewAppendColumn grid col
     cell <- cellRendererTextNew
     treeViewColumnPackStart col cell True
     return (col, cell)

main :: IO ()
main = do

  i <- loadMyInput
  let st = computeGameState i
      rows = sortBy (comparing payoffMetric) (payoff i st)

  initGUI

  window <- myWindow

  model <- listStoreNew rows
  grid <- treeViewNewWithModel model

  (nameColumn  , nameCell) <- myColumn grid "Name"
  (metricColumn, metricCell) <- myColumn grid "Metric"
  (saveupColumn, saveupCell) <- myColumn grid "Saveup"
  (buyatColumn, buyatCell) <- myColumn grid "Buy at"
  (deltaColumn, deltaCell) <- myColumn grid "Delta"

  cellLayoutSetAttributes nameColumn nameCell model $ \row ->
    [ cellText := payoffName row ]

  cellLayoutSetAttributes metricColumn metricCell model $ \row ->
    [ cellText := show (floor (payoffMetric row)) ]

  cellLayoutSetAttributes saveupColumn saveupCell model $ \row ->
    [ cellText := show (floor (payoffSaveup row)) ]

  cellLayoutSetAttributes buyatColumn buyatCell model $ \row ->
    [ cellText := prettyNumber ShortSuffix (payoffBuyAt row) ]

  cellLayoutSetAttributes deltaColumn deltaCell model $ \row ->
    [ cellText := prettyNumber ShortSuffix (payoffDelta row) ]

  cpsLabel <- labelNew (Just "Cookies/s:")
  cpsOutput <- labelNew (Just (prettyNumber ShortSuffix (computeCps i st)))

  bankLabel <- labelNew (Just "Bank/Munch")
  bankOutput <- labelNew (Just (prettyNumber ShortSuffix (L.view cookiesBanked i)))
  munchOutput <- labelNew (Just (prettyNumber ShortSuffix (L.view cookiesMunched i)))
  totalOutput <- labelNew (Just (prettyNumber ShortSuffix (L.view cookiesBanked i + L.view cookiesMunched i)))


  ------------------------------------------------------------
  clipboard <- clipboardGet selectionClipboard

  button <- buttonNew
  set button [ buttonLabel := "Load from clipboard" ]
  on button buttonActivated $
    clipboardRequestText clipboard $ \mb ->
      for_ mb $ \txt ->
        do sav <- loadSave txt
           now <- getCurrentTime
           let i = saveFileToGameInput now sav
               st = computeGameState i
               rows = sortBy (comparing payoffMetric) (payoff i st)
           listStoreClear model
           traverse_ (listStoreAppend model) rows

           set cpsOutput   [labelText := prettyNumber ShortSuffix (computeCps i st)]
           set bankOutput  [labelText := prettyNumber ShortSuffix (L.view cookiesBanked i)]
           set munchOutput [labelText := prettyNumber ShortSuffix (L.view cookiesMunched i)]
           set totalOutput [labelText := prettyNumber ShortSuffix (L.view cookiesBanked i + L.view cookiesMunched i)]
  ------------------------------------------------------------

  layout <- gridNew
  gridSetRowSpacing layout 10
  gridSetColumnSpacing layout 10

  gridAttach layout grid      0 0 4 1

  gridAttach layout bankLabel   0 1 1 1
  gridAttach layout bankOutput  1 1 1 1
  gridAttach layout munchOutput 2 1 1 1
  gridAttach layout totalOutput 3 1 1 1

  gridAttach layout cpsLabel  0 2 1 1
  gridAttach layout cpsOutput 1 2 1 1

  gridAttach layout button    0 3 4 1

  set window [ containerChild := layout ]
  widgetShowAll window

  mainGUI
