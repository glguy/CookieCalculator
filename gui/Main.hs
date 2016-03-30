{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell #-}
{-# Language ForeignFunctionInterface #-}

module Main (main) where

import           CookieClicker
import           SaveFormat
import           GameInput
import           Building

import           EmbedStringTH

import qualified Control.Lens as L
import           Data.Foldable
import           Data.Function ((&))
import           Data.List
import           Data.Ord
import           Data.Time
import           Data.IORef
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Builder
import           Foreign

data MyGtkApp = MyGtkApp
  { cpsOutput, wcpsOutput, tcpsOutput
  , bankOutput, munchOutput, totalOutput
  , reserveOutput, reserve7Output, reserveCOutput
  , jackpot7Output, jackpotEOutput, jackpotDOutput
  , chocolateEggOutput, hchipsOutput, effHChipsOutput :: Label

  , payoffModel :: ListStore PayoffRow
  , payoffTable :: TreeView
  , loadButton :: Button
  , mainWindow :: Window
  , iconsPixbuf :: Pixbuf
  , bankRef    :: {-# UNPACK #-} !(IORef Double)
  }

getMyGtkApp :: IO MyGtkApp
getMyGtkApp =
  do builder <- builderNew
     builderAddFromString builder $$(embedString "gui/MainWindow.glade")

     let load name = builderGetObject builder cast name

     payoffTable    <- load "payoffTable"

     cpsOutput      <- load "cpsOutput"
     wcpsOutput     <- load "wcpsOutput"
     tcpsOutput     <- load "tcpsOutput"

     bankOutput     <- load "bankOutput"
     munchOutput    <- load "munchOutput"
     totalOutput    <- load "bankMunchOutput"

     reserveOutput  <- load "reserveOutput"
     reserve7Output <- load "reserve7Output"
     reserveCOutput <- load "reserveCOutput"

     jackpot7Output <- load "jackpot7Output"
     jackpotEOutput <- load "jackpotEOutput"
     jackpotDOutput <- load "jackpotDOutput"

     chocolateEggOutput <- load "chocolateEggOutput"
     hchipsOutput    <- load "hchipsOutput"
     effHChipsOutput <- load "effHChipsOutput"

     loadButton     <- load "loadButton"
     mainWindow     <- load "mainWindow"

     payoffModel    <- listStoreNew []
     set payoffTable [ treeViewModel := payoffModel ]

     bankRef <- newIORef 0

     iconsPixbuf <- loadIcons

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

computeMetric :: PayoffRow -> Double
computeMetric PayoffRow{..} = logBase 2 (payoffCost / payoffDelta)

installColumns :: MyGtkApp -> IO ()
installColumns app =

  do addNameColumn app
     metricCell <- addColumn app "Metric" (prettyNumber ShortSuffix . computeMetric)
     addCostColumn app
     deltaCell <- addColumn app "Delta"  (prettyNumber ShortSuffix . (*100) . payoffDelta)

     traverse_ (`set` [ cellXAlign := 1 ]) [ metricCell, deltaCell ]

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

addNameColumn :: MyGtkApp -> IO ()
addNameColumn MyGtkApp{payoffTable, payoffModel, iconsPixbuf} =

  do col <- treeViewColumnNew
     treeViewAppendColumn payoffTable col

     pixcell <- cellRendererPixbufNew
     textcell <- cellRendererTextNew

     treeViewColumnPackStart col pixcell False
     treeViewColumnPackStart col textcell True

     cellLayoutSetAttributeFunc col pixcell payoffModel $ \iter ->
        do row <- treeModelGetRow payoffModel iter
           let (c,r) = payoffIcon row
           icon <- pixbufNewSubpixbuf iconsPixbuf (24*c) (24*r) 24 24
           set pixcell [ cellPixbuf := icon ]

     cellLayoutSetAttributeFunc col textcell payoffModel $ \iter ->
        do row <- treeModelGetRow payoffModel iter
           set textcell [ cellText := payoffName row ]

addCostColumn :: MyGtkApp -> IO CellRendererProgress
addCostColumn MyGtkApp{payoffTable, payoffModel, bankRef} =

  do col <- treeViewColumnNew
     set col [ treeViewColumnTitle := "Cost"
             , treeViewColumnMinWidth := 150
             ]
     treeViewAppendColumn payoffTable col

     cell <- cellRendererProgressNew
     treeViewColumnPackStart col cell True

     cellLayoutSetAttributeFunc col cell payoffModel $ \iter ->
        do row <- treeModelGetRow payoffModel iter
           bank <- readIORef bankRef
           set cell
             [ cellProgressText  := Just (prettyNumber ShortSuffix (payoffCost row))
             , cellProgressValue := truncate (min 100 (max 0 (bank / payoffCost row * 100)))
             ]

     return cell

installLoadButtonClickHandler :: MyGtkApp -> IO ()
installLoadButtonClickHandler app =
  do clipboard <- clipboardGet selectionClipboard
     on (loadButton app) buttonActivated (loadFromFromClipboard app clipboard)
     return ()

loadFromFromClipboard :: MyGtkApp -> Clipboard -> IO ()
loadFromFromClipboard app clipboard =
  clipboardRequestText clipboard $ \mb ->
    for_ mb             $ \txt ->
    for_ (loadSave txt) $ \sav ->
    loadFromFromSave app sav

loadFromFromSave :: MyGtkApp -> SaveFile -> IO ()
loadFromFromSave MyGtkApp{..} sav =
  do now <- getCurrentTime

     let i    = saveFileToGameInput now sav
         st   = computeGameState i
         rows = sortBy (comparing computeMetric) (payoff i st)

     listStoreClear payoffModel
     traverse_ (listStoreAppend payoffModel) rows
     set payoffTable [ widgetHeightRequest := 100 ]

     let cps     = computeCps i st
         ecps    = computeWrinklerEffect i st * cps
         munched = L.view cookiesMunched i * L.view wrinklerMultiplier st
         banked  = L.view cookiesBanked i

         newChips
           = L.review _Prestige
               (L.view cookiesEarned i + L.view cookiesForfeit i + munched)
           - L.view prestigeLevel i

         chocolateChips
           = L.review _Prestige
               ( L.view cookiesEarned i
               + L.view cookiesForfeit i
               + munched
               + sellOff (i & buildingOwned Prism L.-~ 1) st * 0.85 * 0.05
               )
           - L.view prestigeLevel i

         setOut l n = set l [labelText := prettyNumber ShortSuffix n]

     writeIORef bankRef (banked + munched)

     setOut cpsOutput      cps
     setOut wcpsOutput     (cps * (1-L.views wrinklers fromIntegral i * 0.05))
     setOut tcpsOutput     ecps

     setOut bankOutput     banked
     setOut munchOutput    munched
     setOut totalOutput    (banked + munched)

     setOut reserveOutput  (6000 * cps)
     setOut reserve7Output (7 * 6000 * cps)
     setOut reserveCOutput (cpsToChainReserve6 cps)

     setOut jackpot7Output (7 * 900 * cps)
     setOut jackpotEOutput (ecps * 666 * ceil' (6 * 2 * 1.1)) -- XXX: generalize
     setOut jackpotDOutput (ecps * 666 * ceil' (6 * 2 * 1.1) + 666 * 60 * cps)

     setOut chocolateEggOutput chocolateChips
     setOut hchipsOutput newChips
     setOut effHChipsOutput (chocolateChips + L.view heavenlyChips i)

class GObjectClass a => GObjectCast a where cast :: GObject -> a
instance GObjectCast Window   where cast = castToWindow
instance GObjectCast Label    where cast = castToLabel
instance GObjectCast Button   where cast = castToButton
instance GObjectCast TreeView where cast = castToTreeView

foreign import ccall "&" cookie_clicker_icons :: Ptr InlineImage

loadIcons :: IO Pixbuf
loadIcons =
  do pixbuf <- pixbufNewFromInline cookie_clicker_icons
     w <- pixbufGetWidth pixbuf
     h <- pixbufGetHeight pixbuf
     pixbufScaleSimple pixbuf (w`quot`2) (h`quot`2) InterpBilinear
