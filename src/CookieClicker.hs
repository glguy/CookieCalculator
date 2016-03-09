{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language ForeignFunctionInterface #-}
{-# Language RankNTypes #-}

module Main (main) where

import GameInput
import Building
import SaveFormat
import SourceData

import Data.Aeson
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Lens hiding (ReifiedPrism(..), prism)
import Data.List
import Data.Fixed
import Data.Ord
import Data.Maybe
import Data.Foldable
import Data.Time
import Foreign.C.Types (CDouble(..))
import Numeric
import Text.Read
import Spreadsheet
import Spreadsheet.Renderer
import Spreadsheet.Sorting
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

initialBuildingStat :: Double -> BuildingStat
initialBuildingStat base = BuildingStat
  { _bldgBase = base
  , _bldgMult = 1
  , _bldgBonus = 0
  , _bldgFree = 0
  }

initialGameState :: GameInput -> GameState
initialGameState input = GameState
  { _buildingStats           = initialBuildingStat <$> baseCps
  , _multiplier              = 1
  , _eggMultipliers          = 0
  , _mouseBonus              = 0
  , _mouseMultiplier         = 1
  , _prestigeMultiplier      = 0
  , _bonusCps                = 0
  , _buildingCostMultiplier  = 1
  , _upgradeCostMultiplier   = 1
  , _milkMultiplier          = 1
  , _milkFactors             = []
  }

baseCps :: Map Building Double
baseCps = Map.fromList
  [ (Cursor,              0.1e0)
  , (Grandma,             1.0e0)
  , (Farm,                8.0e0)
  , (Mine,               47.0e0)
  , (Factory,           260.0e0)
  , (Bank,                1.4e3)
  , (Temple,              7.8e3)
  , (WizardTower,        44.0e3)
  , (Shipment,          260.0e3)
  , (AlchemyLab,          1.6e6)
  , (Portal,             10.0e6)
  , (TimeMachine,        65.0e6)
  , (Antimatter,        430.0e6)
  , (Prism,              2.90e9)
  ]

initialCosts :: Map Building Double
initialCosts = Map.fromList
  [ (Cursor,             15.0e0)
  , (Grandma,           100.0e0)
  , (Farm,                1.1e3)
  , (Mine,               12.0e3)
  , (Factory,           130.0e3)
  , (Bank,              1.400e6)
  , (Temple,             20.0e6)
  , (WizardTower,       330.0e6)
  , (Shipment,            5.1e9)
  , (AlchemyLab,         75.0e9)
  , (Portal,             1.0e12)
  , (TimeMachine,       14.0e12)
  , (Antimatter,       170.0e12)
  , (Prism,              2.1e15)
  ]

computeGameState :: GameInput -> GameState
computeGameState input =
  foldl'
    (\acc a -> view upgradeEffect a input acc)
    (initialGameState input)
    (view upgradesBought input)

computeMilk :: GameInput -> Double
computeMilk = views achievementsEarned $ \x -> fromIntegral x * 0.04

type Effect = GameInput -> GameState -> GameState

mouseAdd :: Effect
mouseAdd = \_ -> mouseBonus +~ 0.01

kittenBonus :: Double -> Effect
kittenBonus pct = \inp -> milkFactors %~ (pct/100:)

cookieBonus :: Double -> Effect
cookieBonus pct = \_ -> multiplier *~ (1+pct/100)

cursorAdd :: Double -> Effect
cursorAdd bonus = \inp ->
  let count = sum (Map.delete Cursor (view buildingsOwned inp))
  in buildingBonus Cursor +~ bonus * fromIntegral count

grandmaType :: Building -> Double -> Effect
grandmaType building count = \inp ->
    let bonus = views (buildingOwned Grandma) fromIntegral inp / count
    in (buildingMult building *~ (1 + 0.01 * bonus))
     . (buildingMult Grandma  *~ 2)

doubler :: Building -> GameInput -> GameState -> GameState
doubler k _ = buildingMult k *~ 2

computeBuildingStatCps :: BuildingStat -> Double
computeBuildingStatCps stat = stat^.bldgBonus + stat^.bldgMult * stat^.bldgBase


computeBuildingCps :: GameState -> Map Building Double
computeBuildingCps st = computeBuildingStatCps <$> view buildingStats st


buildingCosts :: GameInput -> GameState -> Map Building Double
buildingCosts inp st
  = fmap (* view buildingCostMultiplier st)
  $ Map.mergeWithKey
      (\_ cnt base -> Just (base * 1.15 ^ cnt))
      (\_          -> Map.empty)
      id
      owned'
      initialCosts
  where
  owned' = Map.unionWith (-)
             (view buildingsOwned inp)
             (view bldgFree <$> view buildingStats st)

payoff :: GameInput -> GameState -> String
payoff inp st =
  renderSpreadsheet
  $ sortSpreadsheet
  $ Spreadsheet [ Column "Action" StringT Nothing
                , Column "Metric" (NumberT (Just 0)) (Just Ascending)
                , Column "Saveup" (NumberT (Just 1)) Nothing
                , Column "Buy at" StringT Nothing
                , Column "∆C/s" StringT Nothing
                ]
     [ [ StringV act
       , NumberV (toRational (cost / delta))
       , NumberV (toRational (cost / (7*900*cps)))
       , StringV (prettyNumber ShortSuffix (cost + frenzyReserve))
       , StringV (prettyNumber ShortSuffix delta)
       ]
     | (act, cost, f) <- buyBuilding ++ buyUpgrades ++ custom
     , let delta = effect f
     , delta > 0
     ]

  where
  frenzyReserve = 7 * 6000 * cps

  fingersNeeded = 240 - view (buildingOwned Cursor) inp
  fingersCost   = sum $ take (fromIntegral fingersNeeded) $ iterate (*1.15) $ costs ^?! ix Cursor
  custom =
    [ finishA 300 Temple
    , finishA 300 Factory
    , finishA 400 Cursor
    , finish 1 250 WizardTower "Rabbit trick"
    , finish 1 250 Shipment "The final frontier"
    , finish 1 250 AlchemyLab "Beige goo"
    , finish 1 200 Portal "End of back-up plan"
    , finish 1 200 TimeMachine "Great loop hypothesis"
    , finish 1 200 Antimatter "The Pulse"
    , finish 1 150 Prism "Glow-in-the-dark"
    ]

  buyBuilding =
    [( "+1 " ++ show x
     , costs ^?! ix x
     , buildingOwned x +~ 1
     )
    | x <- [Cursor ..] ]

  buyUpgrades =
     [ ( views upgradeName Text.unpack u
       , view upgradeCost u * view upgradeCostMultiplier st
       , upgradesBought <>~ [u]
       )
     | u <- view upgradesAvailable inp
     ]

  costs = buildingCosts inp st
  cps   = computeCps inp st

  effect f = computeCps (f inp) (computeGameState (f inp)) - cps

  finish a n b up = ("+" ++ show n' ++ " " ++ show b, cost, f)
    where
    Just u = Map.lookup up upgradeMap
    n' = n - view (buildingOwned b) inp
    cost = view upgradeCost u + sum (take (fromIntegral n') (iterate (*1.15) (costs ^?! ix b)))
    f = (upgradesBought <>~ [u])
      . (achievementsEarned +~ a)
      . (buildingOwned b .~ n)

  finishA n b = ("+" ++ show n' ++ " " ++ show b, cost, f)
    where
    n' = n - view (buildingOwned b) inp
    cost = sum (take (fromIntegral n') (iterate (*1.15) (costs ^?! ix b)))
    f = (achievementsEarned +~ 1)
      . (buildingOwned b .~ n)

computeMultiplier :: GameInput -> GameState -> Double
computeMultiplier inp st =
  view multiplier st *
  milkFactor *
  (view eggMultipliers     st / 100 + 1) *
  (views prestigeMultiplier fromIntegral st / 100 *
   views prestigeLevel      fromIntegral inp / 100 + 1)
  where
  milkFactor = product [ 1 + milk * x * view milkMultiplier st | x <- view milkFactors st ]
  milk = 0.04 * views achievementsEarned fromIntegral inp

computeCps :: GameInput -> GameState -> Double
computeCps inp st =
  computeMultiplier inp st *
  (view bonusCps st +
  sum (Map.intersectionWith (\count cps -> fromIntegral count * cps)
         (view buildingsOwned inp)
         (computeBuildingCps st))
  )

computeClickCookies :: GameInput -> GameState -> Double
computeClickCookies inp st =
  view mouseMultiplier st *
  (
    computeCps inp st * view mouseBonus st
  + computeMultiplier inp st * (view (buildingMult Cursor) st + view (buildingBonus Cursor) st))

main :: IO ()
main =
  do now <- getCurrentTime
     input <- saveFileToGameInput now <$> loadMySave
     let st = computeGameState input
     putStrLn (payoff input st)
     let cps = computeCps input st
     putStrLn $ "Buildings:\t"   ++ show (sum (view buildingsOwned input))
     putStrLn $ "Munched:\t"     ++ prettyNumber LongSuffix (view cookiesMunched input)
     putStrLn $ "Cookie/s:\t"    ++ prettyNumber LongSuffix cps
     putStrLn $ "Cookie/c:\t"    ++ prettyNumber LongSuffix (computeClickCookies input st)
     putStrLn $ "Reserve:\t"     ++ prettyNumber LongSuffix (7*6000*cps)
     putStrLn $ "Cookie/s x7:\t" ++ prettyNumber LongSuffix (7*cps)
     putStrLn $ "Lucky x7:\t"    ++ prettyNumber LongSuffix (7*900*cps)

data SuffixLength = LongSuffix | ShortSuffix

prettyNumber :: SuffixLength -> Double -> String
prettyNumber s n
  | n < 1e3 = showFFloat (Just 1) n ""
  | n < 1e6 = let (w,p) = properFraction n
              in numberWithSeparators w ++ drop 1 (showFFloat (Just 1) p "")
  | n < 1e9   = showFFloat (Just 3) (n / 1e6 ) (suffix " M" " million")
  | n < 1e12  = showFFloat (Just 3) (n / 1e9 ) (suffix " B" " billion")
  | n < 1e15  = showFFloat (Just 3) (n / 1e12) (suffix " T" " trillion")
  | n < 1e18  = showFFloat (Just 3) (n / 1e15) (suffix " Qa" " quadrillion")
  | n < 1e21  = showFFloat (Just 3) (n / 1e18) (suffix " Qi" " quintillion")
  | n < 1e24  = showFFloat (Just 3) (n / 1e21) (suffix " Sx" " sextillion")
  | n < 1e27  = showFFloat (Just 3) (n / 1e24) (suffix " Sp" " septillion")
  | n < 1e30  = showFFloat (Just 3) (n / 1e27) (suffix " Oc" " octillion")
  | n < 1e33  = showFFloat (Just 3) (n / 1e30) (suffix " No" " nonillion")
  | n < 1e36  = showFFloat (Just 3) (n / 1e33) (suffix " Dc" " decillion")
  | n < 1e39  = showFFloat (Just 3) (n / 1e36) (suffix " UnD" " undecillion")
  | n < 1e42  = showFFloat (Just 3) (n / 1e39) (suffix " DoD" " duodecillion")
  | n < 1e45  = showFFloat (Just 3) (n / 1e42) (suffix " TrD" " tredecillion")
  | n < 1e48  = showFFloat (Just 3) (n / 1e45) (suffix " QaD" " quattuordecillion")
  | otherwise = let (w,p) = properFraction (n / 1e48)
                in numberWithSeparators w
                ++ drop 1 (showFFloat (Just 3) p (suffix " QiD" " quindecillion"))
  where
  suffix short long =
    case s of
      ShortSuffix -> short
      LongSuffix  -> long

numberWithSeparators :: Integer -> String
numberWithSeparators
  = reverse
  . intercalate ","
  . takeWhile (not . null)
  . map     (take 3)
  . iterate (drop 3)
  . reverse
  . show

prettyTime :: Integer -> String
prettyTime t = part y 'y' (y  > 0)
             $ part d 'd' (d' > 0)
             $ part h 'h' (h' > 0)
             $ part m 'm' (m' > 0)
             $ part s 's' True ""
  where
  (m',s) = quotRem t  60
  (h',m) = quotRem m' 60
  (d',h) = quotRem h' 24
  (y ,d) = quotRem d' 365

  part _ _ False = id
  part n c True  = shows n . showChar c

upgradeMap :: Map Text Upgrade
upgradeMap = Map.fromList [ (view upgradeName u, u) | u <- allUpgrades ]

gpoc :: Building -> Double -> Effect
gpoc b bonus = \inp ->
  let gmas = views (buildingOwned b) fromIntegral inp
  in buildingBase Grandma +~ bonus * gmas

allUpgrades :: [Upgrade]
allUpgrades =
   [ Upgrade "Reinforced index finger"        100.0e0  $ doubler Cursor
   , Upgrade "Carpal tunnel prevention cream" 500.0e0  $ doubler Cursor
   , Upgrade "Ambidextrous"                    10.0e3  $ doubler Cursor
   , Upgrade "Thousand fingers"               100.0e3  $ cursorAdd 0.1
   , Upgrade "Million fingers"                 10.0e6  $ cursorAdd 0.5
   , Upgrade "Billion fingers"                100.0e6  $ cursorAdd 5
   , Upgrade "Trillion fingers"                 1.0e9  $ cursorAdd 50
   , Upgrade "Quadrillion fingers"             10.0e9  $ cursorAdd 500
   , Upgrade "Quintillion fingers"             10.0e12 $ cursorAdd 5000
   , Upgrade "Sextillion fingers"             100.0e12 $ cursorAdd 50000
   , Upgrade "Septillion fingers"               1.0e15 $ cursorAdd 500000
   , Upgrade "Octillion fingers"               10.0e15 $ cursorAdd 5000000

       ---- MICE
   , Upgrade "Plastic mouse"     50.0e3  mouseAdd
   , Upgrade "Iron mouse"         5.0e6  mouseAdd
   , Upgrade "Titanium mouse"   500.0e6  mouseAdd
   , Upgrade "Adamantium mouse"  50.0e9  mouseAdd
   , Upgrade "Unobtainium mouse"  5.0e12 mouseAdd
   , Upgrade "Eludium mouse"    500.0e12 mouseAdd
   , Upgrade "Wishalloy mouse"   50.0e15 mouseAdd
   , Upgrade "Fantasteel mouse"   5.0e18 mouseAdd
   , Upgrade "Nevercrack mouse"  50.0e15 mouseAdd

   --    -- GRANDMAS
   , Upgrade "Forwards from grandma"       1.0e3  $ doubler Grandma
   , Upgrade "Steel-plated rolling pins"   5.0e3  $ doubler Grandma
   , Upgrade "Lubricated dentures"        50.0e3  $ doubler Grandma
   , Upgrade "Prune juice"                 5.0e6  $ doubler Grandma
   , Upgrade "Double-thick glasses"      500.0e6  $ doubler Grandma
   , Upgrade "Aging agents"               50.0e9  $ doubler Grandma
   , Upgrade "Xtreme walkers"             50.0e12 $ doubler Grandma
   , Upgrade "The Unbridling"             50.0e15 $ doubler Grandma

   , Upgrade "Farmer grandmas"     55.0e3  $ grandmaType Farm         1
   , Upgrade "Miner grandmas"     600.0e3  $ grandmaType Mine         2
   , Upgrade "Worker grandmas"      6.5e6  $ grandmaType Factory      3
   , Upgrade "Banker grandmas"     70.0e6  $ grandmaType Bank         4
   , Upgrade "Priestess grandmas"   1.0e9  $ grandmaType Temple       5
   , Upgrade "Witch grandmas"      16.5e9  $ grandmaType WizardTower  6
   , Upgrade "Cosmic grandmas"    255.0e9  $ grandmaType Shipment     7
   , Upgrade "Transmuted grandmas" 3.75e12 $ grandmaType AlchemyLab   8
   , Upgrade "Altered grandmas"    50.0e12 $ grandmaType Portal       9
   , Upgrade "Grandmas' grandmas" 700.0e12 $ grandmaType TimeMachine 10
   , Upgrade "Antigrandmas"         8.5e15 $ grandmaType Antimatter  11
   , Upgrade "Rainbow grandmas"   105.0e15 $ grandmaType Prism       12

   --    -- FARMS
   , Upgrade "Cheap hoes"                    11.0e3  $ doubler Farm
   , Upgrade "Fertilizer"                    55.0e3  $ doubler Farm
   , Upgrade "Cookie trees"                 550.0e3  $ doubler Farm
   , Upgrade "Genetically-modified cookies"  55.0e6  $ doubler Farm
   , Upgrade "Gingerbread scarecrows"         5.5e9  $ doubler Farm
   , Upgrade "Pulsar sprinklers"            550.0e9  $ doubler Farm
   , Upgrade "Fudge fungus"                 550.0e12 $ doubler Farm
   , Upgrade "Wheat triffids"               550.0e15 $ doubler Farm

   --    -- MINES
   , Upgrade "Sugar gas"      120.0e3  $ doubler Mine
   , Upgrade "Megadrill"      600.0e3  $ doubler Mine
   , Upgrade "Ultradrill"       6.0e6  $ doubler Mine
   , Upgrade "Ultimadrill"    600.0e6  $ doubler Mine
   , Upgrade "H-bomb mining"   60.0e9  $ doubler Mine
   , Upgrade "Coreforge"        6.0e12 $ doubler Mine
   , Upgrade "Planetsplitters"  6.0e15 $ doubler Mine
   , Upgrade "Canola oil wells" 6.0e18 $ doubler Mine

   --    -- FACTORIES
   , Upgrade "Sturdier conveyor belts" 1.3e6  $ doubler Factory
   , Upgrade "Child labor"             6.5e6  $ doubler Factory
   , Upgrade "Sweatshop"              65.0e6  $ doubler Factory
   , Upgrade "Radium reactors"         6.5e9  $ doubler Factory
   , Upgrade "Recombobulators"       650.0e9  $ doubler Factory
   , Upgrade "Deep-bake process"      65.0e12 $ doubler Factory
   , Upgrade "Cyborg workforce"       65.0e15 $ doubler Factory
   , Upgrade "78-hour days"           65.0e18 $ doubler Factory

   --    -- BANKS
   , Upgrade "Taller tellers" 14e6 $ doubler Bank
   , Upgrade "Scissor-resistant credit cards" 70e6 $ doubler Bank
   , Upgrade "Acid-proof vaults" 700e6 $ doubler Bank
   , Upgrade "Chocolate coins" 70e9 $ doubler Bank
   , Upgrade "Exponential interest rates" 7e12 $ doubler Bank
   , Upgrade "Financial zen" 700e12 $ doubler Bank
   , Upgrade "Way of the wallet" 700e15 $ doubler Bank
   , Upgrade "The stuff rationale" 700e18 $ doubler Bank

   --    -- TEMPLES
   , Upgrade "Golden idols" 200e6 $ doubler Temple
   , Upgrade "Sacrifices" 1e9 $ doubler Temple
   , Upgrade "Delicious blessing" 10e9 $ doubler Temple
   , Upgrade "Sun festival" 1e12 $ doubler Temple
   , Upgrade "Enlarged pantheon" 100e12 $ doubler Temple
   , Upgrade "Great Baker in the sky" 10e15 $ doubler Temple
   , Upgrade "Creation myth" 10e18 $ doubler Temple
   , Upgrade "Theocracy" 10e21 $ doubler Temple

   --    -- WIZARDS
   , Upgrade "Pointier hats" 3.3e9 $ doubler WizardTower
   , Upgrade "Beardlier beards" 16.5e9 $ doubler WizardTower
   , Upgrade "Ancient grimoires" 165e9 $ doubler WizardTower
   , Upgrade "Kitchen curses" 16.5e12 $ doubler WizardTower
   , Upgrade "School of sorcery" 1.65e15 $ doubler WizardTower
   , Upgrade "Dark formulas" 165e15 $ doubler WizardTower
   , Upgrade "Cookiemancy" 165e18 $ doubler WizardTower
   , Upgrade "Rabbit trick" 165e21 $ doubler WizardTower

   --    -- SHIPMENTS
   , Upgrade  "Vanilla nebulae" 51e9 $ doubler Shipment
   , Upgrade "Wormholes" 255e9 $ doubler Shipment
   , Upgrade "Frequent flyer" 2.55e12 $ doubler Shipment
   , Upgrade "Warp drive" 255e12 $ doubler Shipment
   , Upgrade "Chocolate monoliths" 25.5e15 $ doubler Shipment
   , Upgrade "Generation ship" 2.55e18 $ doubler Shipment
   , Upgrade "Dyson sphere" 2.55e21 $ doubler Shipment
   , Upgrade "The final frontier" 2.55e24 $ doubler Shipment

   --    -- ALCHEMY
   , Upgrade "Antimony" 750e9 $ doubler AlchemyLab
   , Upgrade "Essence of dough" 3.75e12 $ doubler AlchemyLab
   , Upgrade "True chocolate" 37.5e12 $ doubler AlchemyLab
   , Upgrade "Ambrosia" 3.75e15 $ doubler AlchemyLab
   , Upgrade "Aqua crustulae" 375e15 $ doubler AlchemyLab
   , Upgrade "Origin crucible" 3.75e18 $ doubler AlchemyLab
   , Upgrade "Theory of atomic fluidity" 37.5e21 $ doubler AlchemyLab
   , Upgrade "Beige goo" 37.5e24 $ doubler AlchemyLab

   --    -- PORTAL
   , Upgrade "Ancient tablet" 10e12 $ doubler Portal
   , Upgrade "Insane oatling workers" 50e12 $ doubler Portal
   , Upgrade "Soul bond" 500e12 $ doubler Portal
   , Upgrade "Sanity dance" 50e15 $ doubler Portal
   , Upgrade "Brane transplant" 5e18 $ doubler Portal
   , Upgrade "Deity-sized portals" 500e18 $ doubler Portal
   , Upgrade "End of back-up plan" 500e21 $ doubler Portal
   , Upgrade "Maddening chants" 500e24 $ doubler Portal

   -- TIME MACHINE
   , Upgrade "Flux capacitors" 140e12 $ doubler TimeMachine
   , Upgrade "Time paradox resolver" 700e12 $ doubler TimeMachine
   , Upgrade "Quantum conundrum" 7e15 $ doubler TimeMachine
   , Upgrade "Causality enforcer" 700e15 $ doubler TimeMachine
   , Upgrade "Yestermorrow comparators" 70e18 $ doubler TimeMachine
   , Upgrade "Far future enactment" 7e21 $ doubler TimeMachine
   , Upgrade "Great loop hypothesis" 7e24 $ doubler TimeMachine
   , Upgrade "Cookietopian moments of maybe" 7e27 $ doubler TimeMachine

   -- ANTIMATTER CONDENSER
   , Upgrade "Sugar bosons" 1.7e15 $ doubler Antimatter
   , Upgrade "String theory" 8.5e15 $ doubler Antimatter
   , Upgrade "Large macaron collider" 85e15 $ doubler Antimatter
   , Upgrade "Big bang bake" 8.5e18 $ doubler Antimatter
   , Upgrade "Reverse cyclotrons" 850e18 $ doubler Antimatter
   , Upgrade "Nanocosmics" 85e21 $ doubler Antimatter
   , Upgrade "The Pulse" 85e24 $ doubler Antimatter
   , Upgrade "Some other super-tiny fundamental particle? Probably?" 85e27 $ doubler Antimatter

   -- PRISM
   , Upgrade "Gem polish"          21e15 $ doubler Prism
   , Upgrade "9th color"          105e15 $ doubler Prism
   , Upgrade "Chocolate light"   1.05e18 $ doubler Prism
   , Upgrade "Grainbow"           105e18 $ doubler Prism
   , Upgrade "Pure cosmic light" 10.5e21 $ doubler Prism
   , Upgrade "Glow-in-the-dark"  1.05e24 $ doubler Prism
   , Upgrade "Lux sanctorum"     1.05e27 $ doubler Prism
   , Upgrade "Reverse shadows"   1.05e30 $ doubler Prism

   --    -- KITTENS
   , Upgrade  "Kitten helpers"       9.0e6  $ kittenBonus 10
   , Upgrade  "Kitten workers"       9.0e9  $ kittenBonus 12.5
   , Upgrade  "Kitten engineers"    90.0e12 $ kittenBonus 15
   , Upgrade "Kitten overseers"    90.0e15 $ kittenBonus 17.5
   , Upgrade "Kitten managers"    900.0e18 $ kittenBonus 20
   , Upgrade "Kitten accountants" 900.0e21 $ kittenBonus 20
   , Upgrade "Kitten specialists" 900.0e24 $ kittenBonus 20
   , Upgrade "Kitten experts"     900.0e27 $ kittenBonus 20
   -- , Upgrade "Kitten angels" 9000HC

   --    -- COOKIES
   , Upgrade "Plain cookies"                      999999    $ cookieBonus 1
   , Upgrade "Sugar cookies"                           5e6  $ cookieBonus 1
   , Upgrade "Oatmeal raisin cookies"                 10e6  $ cookieBonus 1
   , Upgrade "Peanut butter cookies"                  50e6  $ cookieBonus 1
   , Upgrade "Coconut cookies"                       100e6  $ cookieBonus 1
   , Upgrade "White chocolate cookies"               500e6  $ cookieBonus 2
   , Upgrade "Macadamia nut cookies"                   1e9  $ cookieBonus 2
   , Upgrade "Double-chip cookies"                     5e9  $ cookieBonus 2
   , Upgrade "White chocolate macadamia nut cookies"  10e9  $ cookieBonus 2
   , Upgrade "All-chocolate cookies"                  50e9  $ cookieBonus 2
   , Upgrade "Dark chocolate-coated cookies"         100e9  $ cookieBonus 4
   , Upgrade "White chocolate-coated cookies"        100e9  $ cookieBonus 4
   , Upgrade "Eclipse cookies"                       500e9  $ cookieBonus 2
   , Upgrade "Zebra cookies"                           1e12 $ cookieBonus 2
   , Upgrade "Snickerdoodles"                          5e12 $ cookieBonus 2
   , Upgrade "Stroopwafels"                           10e12 $ cookieBonus 2
   , Upgrade "Macaroons"                              50e12 $ cookieBonus 2
   , Upgrade "Empire biscuits"                       100e12 $ cookieBonus 2
   , Upgrade "Madeleines"                            500e12 $ cookieBonus 2
   , Upgrade "Palmiers"                              500e12 $ cookieBonus 2
   , Upgrade "Palets"                                  1e15 $ cookieBonus 2
   , Upgrade "Sablés"                                  1e15 $ cookieBonus 2
   , Upgrade "Caramoas"                               10e15 $ cookieBonus 3
   , Upgrade "Sagalongs"                              10e15 $ cookieBonus 3
   , Upgrade "Shortfoils"                             10e15 $ cookieBonus 3
   , Upgrade "Win mints"                              10e15 $ cookieBonus 3
   , Upgrade "Gingerbread men"                        10e15 $ cookieBonus 2
   , Upgrade "Gingerbread trees"                      10e15 $ cookieBonus 2
   , Upgrade "Pure black chocolate cookies"           50e15 $ cookieBonus 4
   , Upgrade "Pure white chocolate cookies"           50e15 $ cookieBonus 4
   , Upgrade "Ladyfingers"                           100e15 $ cookieBonus 3
   , Upgrade "Tuiles"                                500e15 $ cookieBonus 3
   , Upgrade "Chocolate-stuffed biscuits"              1e18 $ cookieBonus 3
   , Upgrade "Checker cookies"                         5e18 $ cookieBonus 3
   , Upgrade "Butter cookies"                         10e18 $ cookieBonus 3
   , Upgrade "Cream cookies"                          50e18 $ cookieBonus 3
   , Upgrade "Gingersnaps"                           100e18 $ cookieBonus 4
   , Upgrade "Cinnamon cookies"                      500e18 $ cookieBonus 4
   , Upgrade "Vanity cookies"                          1e21 $ cookieBonus 4
   , Upgrade "Cigars"                                  5e21 $ cookieBonus 4
   , Upgrade "Pinwheel cookies"                       10e21 $ cookieBonus 4
   , Upgrade "Fudge squares"                          50e21 $ cookieBonus 4
   , Upgrade "Butter horseshoes"                     100e21 $ cookieBonus 4
   , Upgrade "Shortbread biscuits"                   100e21 $ cookieBonus 4
   , Upgrade "Butter pucks"                          500e21 $ cookieBonus 4
   , Upgrade "Butter knots"                            1e24 $ cookieBonus 4
   , Upgrade "Caramel cookies"                         1e24 $ cookieBonus 4
   , Upgrade "Millionaires' shortbreads"             500e21 $ cookieBonus 4
   , Upgrade "Butter slabs"                            5e24 $ cookieBonus 4
   , Upgrade "Butter swirls"                          10e24 $ cookieBonus 4

   , Upgrade "Milk chocolate butter biscuit" 1.0e21 $ cookieBonus 10

   , Upgrade "Dragon cookie" 0 $ cookieBonus 5

   , Upgrade   "Digits"                                 5e15 $ cookieBonus 2
   , Upgrade   "Jaffa cakes"                            5e15 $ cookieBonus 2
   , Upgrade   "Loreols"                                5e15 $ cookieBonus 2
   , Upgrade   "Fig gluttons"                           5e15 $ cookieBonus 2
   , Upgrade   "Grease's cups"                          5e15 $ cookieBonus 2
   , Upgrade   "Shortfolios"                           10e15 $ cookieBonus 3

   , Upgrade "British tea biscuits" 0 $ cookieBonus 2
   , Upgrade "Chocolate british tea biscuits" 0 $ cookieBonus 2
   , Upgrade "Round british tea biscuits" 0 $ cookieBonus 2
   , Upgrade "Round chocolate british tea biscuits" 0 $ cookieBonus 2
   , Upgrade "Round british tea biscuits with heart motif" 0 $ cookieBonus 2
   , Upgrade "Round chocolate british tea biscuits with heart motif" 0 $ cookieBonus 2

   , Upgrade "Rose macarons" 10.0e3 $ cookieBonus 3
   , Upgrade "Lemon macarons" 10.0e6 $ cookieBonus 3
   , Upgrade "Chocolate macarons" 10.0e9 $ cookieBonus 3
   , Upgrade "Pistachio macarons" 10.0e12 $ cookieBonus 3
   , Upgrade "Hazelnut macarons" 10.0e15 $ cookieBonus 3
   , Upgrade "Violet macarons" 10.0e18 $ cookieBonus 3
   , Upgrade "Caramel macarons" 10.0e21 $ cookieBonus 3
   , Upgrade "Licorice macarons" 10.0e24 $ cookieBonus 3

   , Upgrade "Lucky day"        777777777 $ const id
   , Upgrade "Serendipity"    77777777777 $ const id
   , Upgrade "Get lucky"   77777777777777 $ const id

   , Upgrade "Bingo center/Research facility" 1e15   $ \_ -> buildingMult Grandma *~ 4
   , Upgrade "Specialized chocolate chips"    100e9  $ cookieBonus 1
   , Upgrade "Designer cocoa beans"           200e9  $ cookieBonus 2
   , Upgrade "Ritual rolling pins"            400e9  $ doubler Grandma
   , Upgrade "Underworld ovens"               800e9  $ cookieBonus 3
   , Upgrade "One mind"                       1.6e9  $ gpoc Grandma 0.02
   , Upgrade "Exotic nuts"                    3.2e9  $ cookieBonus 4
   , Upgrade "Communal brainsweep"            6.4e9  $ gpoc Grandma 0.02
   , Upgrade "Arcane sugar"                   12.8e9 $ cookieBonus 5
   , Upgrade "Elder Pact"                     25.6e9 $ gpoc Portal 0.05
   , Upgrade "Sacrificial rolling pins"       0      noEffect

   , Upgrade "Salmon roe"    0 eggBonus
   , Upgrade "Ant larva"     0 eggBonus
   , Upgrade "Cassowary egg" 0 eggBonus
   , Upgrade "Duck egg"      0 eggBonus
   , Upgrade "Turkey egg"    0 eggBonus
   , Upgrade "Turtle egg"    0 eggBonus
   , Upgrade "Quail egg"     0 eggBonus
   , Upgrade "Robin egg"     0 eggBonus
   , Upgrade "Ostrich egg"   0 eggBonus
   , Upgrade "Shark egg"     0 eggBonus
   , Upgrade "Chicken egg"   0 eggBonus
   , Upgrade "Frogspawn"     0 eggBonus
   , Upgrade "Century egg"   0 addEggTimeBonus
   , Upgrade "Cookie egg"    0 $ \_ -> mouseMultiplier *~ 1.1

   , Upgrade "Faberge egg" 0 $ \_ -> buildingCostMultiplier *~ 0.99
   , Upgrade "\"egg\"" 0 $ \_ -> bonusCps +~ 9

   , Upgrade "A crumbly egg" 0 $ const id

   , Upgrade "Pure heart biscuits" 1e6 $ cookieBonus 2
   , Upgrade "Ardent heart biscuits" 1e9 $ cookieBonus 2
   , Upgrade "Sour heart biscuits" 1e12 $ cookieBonus 2
   , Upgrade "Weeping heart biscuits" 1e15 $ cookieBonus 2
   , Upgrade "Golden heart biscuits" 1e18 $ cookieBonus 2
   , Upgrade "Eternal heart biscuits" 1e21 $ cookieBonus 2

   , Upgrade "Christmas tree biscuits" 252.525e9 $ cookieBonus 2
   , Upgrade "Snowflake biscuits" 252.525e9 $ cookieBonus 2
   , Upgrade "Snowman biscuits" 252.525e9 $ cookieBonus 2
   , Upgrade "Holly biscuits" 252.525e9 $ cookieBonus 2
   , Upgrade "Candy cane biscuits" 252.525e9 $ cookieBonus 2
   , Upgrade "Bell biscuits" 252.525e9 $ cookieBonus 2
   , Upgrade "Present biscuits" 252.525e9 $ cookieBonus 2

   , Upgrade "Heavenly chip secret" 11 $ prestigeBonus 5
   , Upgrade "Heavenly cookie stand" 1111 $ prestigeBonus 20
   , Upgrade "Heavenly bakery" 111111 $ prestigeBonus 25
   , Upgrade "Heavenly confectionery" 1111111 $ prestigeBonus 25
   , Upgrade "Heavenly key" 1 $ prestigeBonus 25

   , Upgrade "A festive hat"              0 noEffect
   , Upgrade "Increased merriness"        0 $ cookieBonus 15
   , Upgrade "Improved jolliness"         0 $ cookieBonus 15
   , Upgrade "A lump of coal"             0 $ cookieBonus 1
   , Upgrade "An itchy sweater"           0 $ cookieBonus 1
   , Upgrade "Reindeer baking grounds"    0 noEffect
   , Upgrade "Weighted sleds"             0 noEffect
   , Upgrade "Ho ho ho-flavored frosting" 0 noEffect
   , Upgrade "Season savings"             0 $ \_ -> buildingCostMultiplier *~ 0.99
   , Upgrade "Toy workshop"               0 $ \_ -> upgradeCostMultiplier *~ 0.95
   , Upgrade "Naughty list"               0 $ doubler Grandma
   , Upgrade "Santa's bottomless bag"     0 noEffect-- drops
   , Upgrade "Santa's helpers"            0 $ \_ -> mouseMultiplier *~ 1.1
   , Upgrade "Santa's legacy"             0 $ cookieBonus (15*3)
   , Upgrade "Santa's milk and cookies"   0 $ \_ -> milkMultiplier *~ 1.05
   , Upgrade "Santa's dominion"           0 $ cookieBonus 20
                                                -- buildings 1
                                                -- upgrades 2

   , Upgrade "Skull cookies"   0 $ cookieBonus 2
   , Upgrade "Ghost cookies"   0 $ cookieBonus 2
   , Upgrade "Bat cookies"     0 $ cookieBonus 2
   , Upgrade "Slime cookies"   0 $ cookieBonus 2
   , Upgrade "Pumpkin cookies" 0 $ cookieBonus 2
   , Upgrade "Eyeball cookies" 0 $ cookieBonus 2
   , Upgrade "Spider cookies"  0 $ cookieBonus 2

   , Upgrade "Future almanacs"              0 $ synergy Farm TimeMachine
   , Upgrade "Seismic magic"                0 $ synergy Mine WizardTower
   , Upgrade "Quantum electronics"          0 $ synergy Factory Antimatter
   , Upgrade "Contracts from beyond"        0 $ synergy Bank Portal
   , Upgrade "Paganism"                     0 $ synergy Temple Portal
   , Upgrade "Arcane knowledge"             0 $ synergy WizardTower AlchemyLab
   , Upgrade "Fossil fuels"                 0 $ synergy Mine Shipment
   , Upgrade "Primordial ores"              0 $ synergy Mine AlchemyLab
   , Upgrade "Infernal crops"               0 $ synergy Farm Portal
   , Upgrade "Relativistic parsec-skipping" 0 $ synergy Shipment TimeMachine
   , Upgrade "Extra physics funding"        0 $ synergy Bank Antimatter
   , Upgrade "Light magic"                  0 $ synergy WizardTower Prism

   -- , Upgrade "Rain prayer" 0 $ synergy Temple Farm
   -- , Upgrade "Asteroid mining" 0 $ synergy Mine Shipment
   -- , Upgrade "Temporal overclocking" 0 $ synergy Factory TimeMachine
   -- , Upgrade "Printing press" 0 $ synergy Bank Factory
   -- , Upgrade "God particle" 0 $ synergy Antimatter Temple
   -- , Upgrade "Magical botany" 0 $ synergy Farm WizardTower
   -- , Upgrade "Shipyards" 0 $ synergy Factory Shipment
   -- , Upgrade "Gold fund" 0 $ synergy Bank AlchemyLab
   -- , Upgrade "Abysmal glimmer" 0 $ synergy Portal Prism

   , Upgrade "Revoke Elder Covenant"       0 noEffect
   , Upgrade "Persistent memory"           0 noEffect
   , Upgrade "Weighted sleighs"            0 noEffect
   , Upgrade "Season switcher"             0 noEffect
   , Upgrade "Bunny biscuit"               0 noEffect
   , Upgrade "Tin of british tea biscuits" 0 noEffect
   , Upgrade "Box of macarons"             0 noEffect
   , Upgrade "Box of brand biscuits"       0 noEffect
   , Upgrade "Permanent upgrade slot I"    0 noEffect
   , Upgrade "Permanent upgrade slot II"   0 noEffect
   , Upgrade "Angels"                      0 noEffect
   , Upgrade "Archangels"                  0 noEffect
   , Upgrade "Virtues"                     0 noEffect
   , Upgrade "Dominions"                   0 noEffect
   , Upgrade "Twin Gates of Transcendence" 0 noEffect
   , Upgrade "Heavenly luck"               0 noEffect
   , Upgrade "Lasting fortune"             0 noEffect

   , Upgrade "Starter kit"     0 $ \_ -> buildingFree Cursor  +~ 10
   , Upgrade "Starter kitchen" 0 $ \_ -> buildingFree Grandma +~  5

   , Upgrade "How to bake your dragon" 0 noEffect
   , Upgrade "Tin of butter cookies" 0 noEffect
   , Upgrade "Golden switch" 0 $ cookieBonus 50
   , Upgrade "Classic dairy selection" 0 noEffect
   , Upgrade "Belphegor" 0 noEffect
   , Upgrade "Mammon" 0 noEffect
   , Upgrade "Abaddon" 0 noEffect
   , Upgrade "Satan" 0 noEffect
   , Upgrade "Legacy" 0 noEffect
   , Upgrade "Synergies Vol. I" 0 noEffect
   , Upgrade "Synergies Vol. II" 0 noEffect
   , Upgrade "Elder Pledge" 0 noEffect
   , Upgrade "Elder Covenant" 0 noEffect
   , Upgrade "Festive biscuit" 0 noEffect
   , Upgrade "Ghostly biscuit" 0 noEffect
   , Upgrade "Lovesick biscuit" 0 noEffect
   , Upgrade "Fool's biscuit" 0 noEffect
   , Upgrade "Golden switch [off]" 0 noEffect
   , Upgrade "Golden switch [on]" 0 noEffect
   , Upgrade "Milk selector" 0 noEffect
   , Upgrade "Golden goose egg" 0 noEffect
   , Upgrade "Chocolate egg" 0 noEffect
   ]

noEffect :: Effect
noEffect _ st = st

eggBonus :: Effect
eggBonus _ = eggMultipliers +~ 1

prestigeBonus :: Int -> Effect
prestigeBonus n _ = prestigeMultiplier +~ n

addEggTimeBonus :: Effect
addEggTimeBonus inp = eggMultipliers +~ views sessionLength eggTimeBonus inp

eggTimeBonus :: Double -> Double
eggTimeBonus s = (1 - (1 - day/100)**3) * 10
  where
  floor' = fromInteger . floor
  day = min 100
      $ floor' (s / 10) * 10 / (60 * 60 * 24)

floor' :: Double -> Double
floor' = realToFrac . c_floor . realToFrac

foreign import ccall "math.h floor" c_floor :: CDouble -> CDouble

synergy :: Building -> Building -> Effect
synergy major minor inp
  = (buildingMult major *~ (1 + 0.05 * fromIntegral minorCount))
  . (buildingMult minor *~ (1 + 0.001 * fromIntegral majorCount))
  where
  majorCount = view (buildingOwned major) inp
  minorCount = view (buildingOwned minor) inp


computePrestige :: Double -> Int
computePrestige cs = floor ((cs/1e12)**(1/3))

saveFileToGameInput :: UTCTime -> SaveFile -> GameInput
saveFileToGameInput now sav = GameInput
  { _buildingsOwned     = bldgCurrent <$> savBuildings sav
  , _achievementsEarned = achievements
  , _upgradesBought     = upgradeList snd
  , _upgradesAvailable  = upgradeList inShop
  , _prestigeLevel      = computePrestige (savForfeited sav)
  , _sessionLength      = realToFrac (diffUTCTime now (savSessionStart sav))
  , _cookiesMunched     = savMunched sav
  }
  where
  idToUpgrade i =
    Map.findWithDefault (error ("Unknown upgrade: " ++ Text.unpack name)) name upgradeMap
    where
    name = upgradeNameById !! i

  inShop (unlocked,bought) = unlocked && not bought

  upgradeList f
     = fmap idToUpgrade
     $ findIndices f
     $ savUpgrades sav

  achievements
    = length
    $ filter (\i -> achievementPoolById !! i == "normal")
    $ findIndices id
    $ savAchievements sav
