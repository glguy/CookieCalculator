{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell #-}

module Main (main) where

import           CookieClicker
import           SaveFormat
import           GameInput

import           EmbedStringTH

import qualified Control.Lens as L
import           Data.Foldable
import           Data.List
import           Data.Ord
import           Data.Time
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Builder



noString :: Maybe String
noString = Nothing

data MyGtkApp = MyGtkApp
  { cpsOutput, wcpsOutput, tcpsOutput
  , bankOutput, munchOutput, totalOutput
  , reserveOutput, reserve7Output, reserveCOutput
  , jackpot7Output, jackpotEOutput, jackpotDOutput :: Label

  , payoffModel :: ListStore PayoffRow
  , payoffTable :: TreeView
  , loadButton :: Button
  , mainWindow :: Window
  }

getMyGtkApp :: IO MyGtkApp
getMyGtkApp =
  do builder <- builderNew
     builderAddFromString builder $$(embedString "gui/MainWindow.glade")

     payoffTable    <- builderGetObject builder castToTreeView "payoffTable"

     cpsOutput      <- builderGetObject builder castToLabel "cpsOutput"
     wcpsOutput     <- builderGetObject builder castToLabel "wcpsOutput"
     tcpsOutput     <- builderGetObject builder castToLabel "tcpsOutput"

     bankOutput     <- builderGetObject builder castToLabel "bankOutput"
     munchOutput    <- builderGetObject builder castToLabel "munchOutput"
     totalOutput    <- builderGetObject builder castToLabel "bankMunchOutput"

     reserveOutput  <- builderGetObject builder castToLabel "reserveOutput"
     reserve7Output <- builderGetObject builder castToLabel "reserve7Output"
     reserveCOutput <- builderGetObject builder castToLabel "reserveCOutput"

     jackpot7Output <- builderGetObject builder castToLabel "jackpot7Output"
     jackpotEOutput <- builderGetObject builder castToLabel "jackpotEOutput"
     jackpotDOutput <- builderGetObject builder castToLabel "jackpotDOutput"

     loadButton     <- builderGetObject builder castToButton "loadButton"
     mainWindow     <- builderGetObject builder castToWindow "mainWindow"

     payoffModel    <- listStoreNew []
     set payoffTable [ treeViewModel := payoffModel ]

     return MyGtkApp{..}

main :: IO ()
main =
  do initGUI
     app <- getMyGtkApp

     installColumns app
     installLoadButtonClickHandler app

     on (mainWindow app) objectDestroy mainQuit
     widgetShowAll (mainWindow app)

     mainGUI

installColumns :: MyGtkApp -> IO ()
installColumns app =

  do nameCell   <- addColumn app "Name"   payoffName
     metricCell <- addColumn app "Metric" (prettyNumber ShortSuffix . logBase 2 . payoffMetric)
     saveupCell <- addColumn app "Saveup" (prettyNumber ShortSuffix . payoffSaveup )
     buyatCell  <- addColumn app "Buy at" (prettyNumber ShortSuffix . payoffBuyAt)
     deltaCell  <- addColumn app "Delta"  (prettyNumber ShortSuffix . payoffDelta)

     let attrs = [ cellXAlign := 1, cellTextFamily := "Monospace", cellTextSize := 18 ]
     traverse_ (`set` attrs) [ metricCell, saveupCell, buyatCell, deltaCell ]

addColumn :: MyGtkApp -> String -> (PayoffRow -> String) -> IO CellRendererText
addColumn MyGtkApp{payoffTable, payoffModel} name render =

  do col <- treeViewColumnNew
     set col [ treeViewColumnTitle := name ]
     treeViewAppendColumn payoffTable col

     cell <- cellRendererTextNew
     treeViewColumnPackStart col cell True
     cellLayoutSetAttributes col cell payoffModel $ \row ->
       [ cellText := render row ]

     return cell

installLoadButtonClickHandler :: MyGtkApp -> IO ()
installLoadButtonClickHandler app =
  do clipboard <- clipboardGet selectionClipboard
     on (loadButton app) buttonActivated (loadFormFromClipboard app clipboard)
     return ()

loadFormFromClipboard :: MyGtkApp -> Clipboard -> IO ()
loadFormFromClipboard app clipboard =
  clipboardRequestText clipboard $ \mb ->
    for_ mb             $ \txt ->
    for_ (loadSave txt) $ \sav ->
    loadFormFromSave app sav

loadFormFromSave :: MyGtkApp -> SaveFile -> IO ()
loadFormFromSave MyGtkApp{..} sav =
  do now <- getCurrentTime

     let i    = saveFileToGameInput now sav
         st   = computeGameState i
         rows = sortBy (comparing payoffMetric) (payoff i st)

     listStoreClear payoffModel
     traverse_ (listStoreAppend payoffModel) rows

     let cps     = computeCps i st
         ecps    = computeWrinklerEffect i st * cps
         munched = L.view cookiesMunched i
         banked  = L.view cookiesBanked i

     let setOut l n = set l [labelText := prettyNumber ShortSuffix n]

     setOut cpsOutput      $ cps
     setOut wcpsOutput     $ ecps-cps
     setOut tcpsOutput     $ ecps

     setOut bankOutput     $ banked
     setOut munchOutput    $ munched
     setOut totalOutput    $ banked + munched

     setOut reserveOutput  $ 6000 * cps
     setOut reserve7Output $ 7 * 6000 * cps
     setOut reserveCOutput $ cpsToChainReserve6 cps

     setOut jackpot7Output $ 7 * 900 * cps
     setOut jackpotEOutput $ ecps * 666 * ceil' (6 * 2 * 1.1) -- XXX: generalize
     setOut jackpotDOutput $ ecps * 666 * ceil' (6 * 2 * 1.1) + 666 * 60 * cps
