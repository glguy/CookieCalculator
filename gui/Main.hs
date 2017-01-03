{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import           CookieClicker
import           SaveFormat
import           GameInput
import           Building

import qualified Control.Lens as L
import           Data.Foldable (for_, traverse_)
import           Data.Function ((&))
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Time (getCurrentTime)
import           Foreign (Ptr)
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, withForeignPtr)
import           Numeric (showFFloat)
import           Data.Int
import qualified Data.Text as Text
import           Data.Text (Text)

import           GI.Gtk (new, get, set, AttrOp(..))
import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as Gdk
import qualified GI.Gdk as Gdk
import AutoBuilder
import GHC.Generics
import Data.Int (Int32)
import Data.Word (Word8)
import qualified Data.ByteString.Internal as BI
import qualified Data.GI.Base.GValue as GValue
import Data.Coerce

data MyGtkApp = MyGtkApp
  { mainWindow :: Gtk.Window
  , cpsOutput, wcpsOutput, tcpsOutput
  , bankOutput, munchOutput, bankMunchOutput
  , reserveOutput, reserve7Output, reserveCOutput
  , jackpot7Output, jackpotEOutput, jackpotDOutput
  , chocolateEggOutput, hchipsOutput, effHChipsOutput :: Gtk.Label
  , loadButton :: Gtk.Button
  , payoffModel :: Gtk.ListStore
  , payoffTable :: Gtk.TreeView
  }
  deriving Generic

data AppState = AppState
  { gtkApp :: MyGtkApp
  , iconsPixbuf :: Gdk.Pixbuf
  }

getAppState :: IO AppState
getAppState =
  do builder     <- Gtk.builderNewFromResource "/glguy/cookieclicker/MainWindow.ui"
     gtkApp      <- autoloadFromBuilder builder
     iconsPixbuf <- loadIcons

     return AppState{..}

main :: IO ()
main =
  do Gtk.init Nothing

     app <- getAppState

     installColumns app
     installLoadButtonClickHandler app

     Gtk.on (mainWindow (gtkApp app)) #destroy Gtk.mainQuit
     #showAll (mainWindow (gtkApp app))

     Gtk.main

installLoadButtonClickHandler :: AppState -> IO ()
installLoadButtonClickHandler app =
  do selectionClipboard <- Gdk.atomIntern "CLIPBOARD" False
     clipboard <- Gtk.clipboardGet selectionClipboard
     Gtk.on (loadButton (gtkApp app))
        #clicked
        (loadFromFromClipboard app clipboard)
     return ()

loadFromFromClipboard :: AppState -> Gtk.Clipboard -> IO ()
loadFromFromClipboard app clipboard =
  do mb <- #waitForText clipboard
     for_ mb             $ \txt ->
       for_ (loadSave (Text.unpack txt)) $ \sav ->
       loadFromFromSave app sav

-- | Metric based on minimizing: @time/benefit + time@
--
-- Given two possible actions we characterize when it is
-- faster to perform the first action before the second
--
-- t: Time required to perform the first action
-- u: Time required to perform the second action
-- m: Incremental multiplicative benefit derived from first action
-- n: Incremental multiplicative benefit derived from second action
--
-- Assumptions: 0 < t,u,m,n
--
-- time for first then second < time for second then first
-- t + u/(1+m)                < u + t/(1+n)
-- t - t/(1+n)                < u - u/(1+m)
-- t*(1+m)(1+n) - t*(1+m)     < u*(1+m)(1+n) - u*(1+n)
-- t*(1+m)*(1+n-1)            < u*(1+n)*(1+m - 1)
-- t*(1+m)*n                  < u*(1+n)*m
-- t*(1+m)/m                  < u*(1+n)/n
-- t*(1+1/m)                  < u*(1+1/n)
computeMetric :: PayoffRow -> Double
computeMetric PayoffRow{..}
  = (log payoffCost + log1p (recip payoffDelta))
  / log 1.15

-- $ payoffCost/payoffDelta + payoffCost
installColumns :: AppState -> IO ()
installColumns app =

  do addNameColumn app
     addColumn app 1 "Metric"
     addCostColumn app
     addColumn app 3 "Benefit"


addColumn :: AppState -> Int32 -> Text -> IO ()
addColumn app n name =

  do col <- new Gtk.TreeViewColumn [ #title := name ]
     #appendColumn (payoffTable (gtkApp app)) col

     cell <- new Gtk.CellRendererText [ #xalign := 1 ]

     #packStart col cell True
     #addAttribute col cell "text" n

     return ()

addNameColumn :: AppState -> IO ()
addNameColumn app =

  do col <- new Gtk.TreeViewColumn [ #title := "Name" ]
     #appendColumn (payoffTable (gtkApp app)) col

     pixcell  <- new Gtk.CellRendererPixbuf []
     #packStart    col pixcell False
     #addAttribute col pixcell "pixbuf" 5

     textcell <- new Gtk.CellRendererText []
     #packStart    col textcell True
     #addAttribute col textcell "text" 0

     return ()


addCostColumn :: AppState -> IO ()
addCostColumn app =

  do col <- new Gtk.TreeViewColumn
             [ #title    := "Cost"
             , #minWidth := 150
             ]
     #appendColumn (payoffTable (gtkApp app)) col

     cell <- new Gtk.CellRendererProgress []
     #packStart    col cell True
     #addAttribute col cell "text" 2
     #addAttribute col cell "value" 4

iconForRow :: AppState -> PayoffRow -> IO Gdk.Pixbuf
iconForRow app row =
  #newSubpixbuf (iconsPixbuf app)
      (tileSz*fromIntegral c)
      (tileSz*fromIntegral r)
      tileSz tileSz
  where
    (c,r)  = payoffIcon row
    tileSz = 24

appendRow :: AppState -> Double -> PayoffRow -> IO ()
appendRow app bank row =
  do vals <- sequence
        [ Gdk.toGValue (Just (payoffName row))
        , Gdk.toGValue (Just (showFFloat (Just 1) (computeMetric row) ""))
        , Gdk.toGValue (Just (prettyNumber ShortSuffix (payoffCost row)))
        , Gdk.toGValue (Just (prettyPercentage (payoffDelta row)))
        , Gdk.toGValue (truncate (min 100 (max 0 (bank / payoffCost row * 100))) :: Int32)
        , gobjectToGValue =<< iconForRow app row
        ]

     let store = payoffModel (gtkApp app)
     iter <- #append store
     #set store iter [0..5] vals

loadFromFromSave :: AppState -> SaveFile -> IO ()
loadFromFromSave app@AppState{gtkApp=MyGtkApp{..},..} sav =
  do now <- getCurrentTime

     let i    = saveFileToGameInput now sav
         st   = computeGameState i
         rows = sortBy (comparing computeMetric) (payoff i st)

         cps     = computeCps i st
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

         setOut :: Gtk.Label -> Double -> IO ()
         setOut l n = set l [#label := Text.pack (prettyNumber ShortSuffix n)]

     #clear payoffModel
     traverse_ (appendRow app (banked + munched)) rows
     #columnsAutosize payoffTable

     setOut cpsOutput      cps
     setOut wcpsOutput     (cps * (1-L.views wrinklers fromIntegral i * 0.05))
     setOut tcpsOutput     ecps

     setOut bankOutput     banked
     setOut munchOutput    munched
     setOut bankMunchOutput (banked + munched)

     setOut reserveOutput  (6000 * cps)
     setOut reserve7Output (7 * 6000 * cps)
     setOut reserveCOutput (cpsToChainReserve6 cps)

     setOut jackpot7Output (7 * 900 * cps)
     setOut jackpotEOutput (ecps * 666 * ceil' (6 * 2 * 1.1)) -- XXX: generalize
     setOut jackpotDOutput (ecps * 666 * ceil' (6 * 2 * 1.1) + 666 * 60 * cps)

     setOut chocolateEggOutput chocolateChips
     setOut hchipsOutput newChips
     setOut effHChipsOutput (chocolateChips + L.view heavenlyChips i)

prettyPercentage :: Double -> String
prettyPercentage x = prefix ++ prettyPercentage' (abs x)
  where
  prefix | x < 0 = "-"
         | otherwise = ""

-- Helper to 'prettyFractionalNumber' that handles positive values
prettyPercentage' :: Double -> String
prettyPercentage' x
  | x >= 1    = prettyNumber ShortSuffix x
  | x >= 0.01 = showFFloat (Just 1) (x'*100) " %"
  | otherwise = showFFloat (Just 1) (x'*10000) " ‱"
  where
  x' = fixPrecision 3 x

fixPrecision :: Int -> Double -> Double
fixPrecision p x = round' (x * m) / m
  where
  m = 10^^(p-e-1)
  e = floor (logBase 10 x)


loadIcons :: IO Gdk.Pixbuf
loadIcons =
  do pixbuf <- Gdk.pixbufNewFromResource "/glguy/cookieclicker/icons.png"
     w      <- get pixbuf #width
     h      <- get pixbuf #height
     Gdk.pixbufScaleSimple pixbuf (w`quot`2) (h`quot`2) Gdk.InterpTypeBilinear

gobjectToGValue ::
  forall o. (Gdk.ManagedPtrNewtype o, Gdk.GObject o) => o -> IO Gdk.GValue
gobjectToGValue o =
  do ty <- Gdk.gobjectType o
     withForeignPtr (Gdk.managedForeignPtr (coerce o :: Gdk.ManagedPtr o))
       (GValue.buildGValue ty GValue.set_object)
