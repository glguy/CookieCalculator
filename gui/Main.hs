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


myColumn grid model name render =
  do col <- treeViewColumnNew
     set col [ treeViewColumnTitle := name ]
     treeViewAppendColumn grid col
     cell <- cellRendererTextNew
     treeViewColumnPackStart col cell True
     cellLayoutSetAttributes col cell model $ \row ->
       [ cellText := render row ]
     return cell

noString :: Maybe String
noString = Nothing

main :: IO ()
main = do

  initGUI

  model <- listStoreNew []
  grid <- treeViewNewWithModel model

  nameCell   <- myColumn grid model "Name" payoffName
  metricCell <- myColumn grid model "Metric" (prettyNumber ShortSuffix . logBase 2 . payoffMetric)
  saveupCell <- myColumn grid model "Saveup" (prettyNumber ShortSuffix . payoffSaveup )
  buyatCell  <- myColumn grid model "Buy at" (prettyNumber ShortSuffix . payoffBuyAt)
  deltaCell  <- myColumn grid model "Delta"  (prettyNumber ShortSuffix . payoffDelta)

  let attrs = [ cellXAlign := 1, cellTextFamily := "Monospace", cellTextSize := 18 ]
  traverse_ (`set` attrs) [ metricCell, saveupCell, buyatCell, deltaCell ]

  cpsLabel   <- labelNew (Just "Cookies/s:")
  cpsOutput  <- labelNew_ "Cookies per second"
  wcpsOutput <- labelNew_ "Munched cookies per second"
  tcpsOutput <- labelNew_ "Total cookies per second"

  bankLabel   <- labelNew (Just "Bank/Munch:")
  bankOutput  <- labelNew_ "Cookies in bank"
  munchOutput <- labelNew_ "Cookies munched"
  totalOutput <- labelNew_ "Total cookies"

  reserveLabel   <- labelNew (Just "Reserve (1/7/C):")
  reserveOutput  <- labelNew_ "Lucky!"
  reserve7Output <- labelNew_ "7x Lucky!"
  reserveCOutput <- labelNew_ "Chain"


  ------------------------------------------------------------

  clipboard <- clipboardGet selectionClipboard

  button <- buttonNew
  set button [ buttonLabel := "Load from clipboard" ]

  on button buttonActivated $
    clipboardRequestText clipboard $ \mb ->
      for_ mb $ \txt ->
      for_ (loadSave txt) $ \sav ->
        do now <- getCurrentTime
           let i = saveFileToGameInput now sav
               st = computeGameState i
               rows = sortBy (comparing payoffMetric) (payoff i st)
           listStoreClear model
           traverse_ (listStoreAppend model) rows

           let cps     = computeCps i st
               ecps    = computeWrinklerEffect i st * cps
               munched = L.view cookiesMunched i
               banked  = L.view cookiesBanked i
           set cpsOutput   [labelText := prettyNumber ShortSuffix cps]
           set wcpsOutput  [labelText := prettyNumber ShortSuffix (ecps-cps)]
           set tcpsOutput  [labelText := prettyNumber ShortSuffix ecps]

           set bankOutput  [labelText := prettyNumber ShortSuffix (banked)]
           set munchOutput [labelText := prettyNumber ShortSuffix munched]
           set totalOutput [labelText := prettyNumber ShortSuffix (banked + munched)]

           set reserveOutput [labelText := prettyNumber ShortSuffix (6000 * cps)]
           set reserve7Output [labelText := prettyNumber ShortSuffix (7 * 6000 * cps)]
           set reserveCOutput [labelText := prettyNumber ShortSuffix (cpsToChainReserve6 cps)]

  -- LAYOUT GRID ---------------------------------------------

  layout <- gridNew

  gridSetRowSpacing    layout 10
  gridSetColumnSpacing layout 10

  gridAttach layout grid      0 0 4 1

  gridAttach layout bankLabel   0 1 1 1
  gridAttach layout bankOutput  1 1 1 1
  gridAttach layout munchOutput 2 1 1 1
  gridAttach layout totalOutput 3 1 1 1

  gridAttach layout cpsLabel   0 2 1 1
  gridAttach layout cpsOutput  1 2 1 1
  gridAttach layout wcpsOutput 2 2 1 1
  gridAttach layout tcpsOutput 3 2 1 1

  gridAttach layout reserveLabel  0 3 1 1
  gridAttach layout reserveOutput 1 3 1 1
  gridAttach layout reserve7Output 2 3 1 1
  gridAttach layout reserveCOutput 3 3 1 1

  gridAttach layout button    0 4 4 1

  -- WINDOW --------------------------------------------------

  window <- windowNew
  on window objectDestroy mainQuit
  set window
       [ containerBorderWidth := 10
       , windowTitle          := "Cookie Calculator"
       , windowResizable      := False
       , containerChild       := layout
       ]
  widgetShowAll window

  mainGUI

labelNew_ tooltip =
  do l <- labelNew (Nothing :: Maybe String)
     set l [ widgetTooltipText := Just tooltip ]
     return l
