{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}

module Main (main) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Lens hiding (ReifiedPrism(..), prism)
import Data.List
import Data.Fixed
import Data.Ord
import Data.Maybe
import Data.Foldable
import Numeric
import Text.Read
import Spreadsheet
import Spreadsheet.Renderer
import Spreadsheet.Sorting
import qualified Config
import qualified Config.Lens as Config
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

data Building
  = Cursor
  | Grandma
  | Farm
  | Mine
  | Factory
  | Bank
  | Temple
  | WizardTower
  | Shipment
  | AlchemyLab
  | Portal
  | TimeMachine
  | Antimatter
  | Prism
  deriving (Read, Show, Eq, Ord, Enum)

data Upgrade = Upgrade
  { _upgradeId     :: Int
  , _upgradeName   :: String
  , _upgradeCost   :: Double
  , _upgradeEffect :: GameInput -> GameState -> GameState
  }

data GameState = GameState
  { _buildingMults   :: !(Map Building Double)
  , _buildingBases   :: !(Map Building Double)
  , _buildingBonuses :: !(Map Building Double)
  , _multiplier      :: !Double
  , _mouseBonus      :: !Double
  }
  deriving (Read, Show)

data GameInput = GameInput
  { _buildingsOwned     :: Map Building Integer
  , _achievementsEarned :: Integer
  , _upgradesBought     :: [Upgrade]
  , _upgradesAvailable  :: [Upgrade]
  }

makeLenses ''GameInput
makeLenses ''GameState
makeLenses ''Upgrade

initialGameState :: GameInput -> GameState
initialGameState input = GameState
  { _buildingBases = baseCps
  , _buildingMults = Map.empty
  , _buildingBonuses = Map.empty
  , _multiplier  = 1
  , _mouseBonus  = 0
  }

buildingMult :: Building -> Lens' GameState Double
buildingMult k = buildingMults . at k . non 1

buildingBonus :: Building -> Lens' GameState Double
buildingBonus k = buildingBonuses . at k . non 0

buildingBase :: Building -> Lens' GameState Double
buildingBase k = buildingBases . singular (ix k)

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
computeGameState input
  = foldl'
      (\acc a -> view upgradeEffect a input acc)
      (initialGameState input)
      (view upgradesBought input)

computeMilk :: GameInput -> Double
computeMilk = views achievementsEarned $ \x -> fromIntegral x * 0.04

type Effect = GameInput -> GameState -> GameState

mouseAdd :: Effect
mouseAdd = \_ -> mouseBonus +~ 0.01

kittenBonus :: Double -> Effect
kittenBonus pct = \inp -> multiplier *~ (1 + computeMilk inp * pct/100)

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

buildingOwned :: Building -> Lens' GameInput Integer
buildingOwned k = buildingsOwned . at k . non 0

doubler :: Building -> GameInput -> GameState -> GameState
doubler k _ = buildingMult k *~ 2

computeBuildingCps :: GameState -> Map Building Double
computeBuildingCps st
  = Map.unionWith (+) multiplied (view buildingBonuses st)
  where

  multiplied =
    Map.mergeWithKey (\_ n m -> Just (n*m))
        id -- default to 1x multiplier
        noStray -- verify no stray multipliers
        (view buildingBases st)
        (view buildingMults st)

  noStray m | Map.null m = Map.empty
            | otherwise  = error "stray multiplier"


buildingCosts :: GameInput -> Map Building Double
buildingCosts inp =
  Map.mergeWithKey
    (\_ cnt base -> Just (base * 1.15 ^ cnt))
    (\_          -> Map.empty)
    id
    (view buildingsOwned inp)
    initialCosts

payoff :: GameInput -> String
payoff inp =
  renderSpreadsheet
  $ sortSpreadsheet
  $ Spreadsheet [ Column "Action" StringT Nothing
                , Column "Metric" (NumberT (Just 0)) (Just Ascending)
                , Column "Saveup" StringT Nothing
                , Column "Buy at" StringT Nothing
                ]
     [ [ StringV act
       , NumberV (toRational pay)
       , StringV (prettyTime (truncate (cost / cps)))
       , StringV (prettyNumber ShortSuffix (cost + frenzyReserve))
       ]
     | (act, (pay, cost)) <- buyBuilding ++ buyUpgrades ++ custom
     ]

  where
  frenzyReserve = 7 * 6000 * cps

  fingersNeeded = 240 - view (buildingOwned Cursor) inp
  fingersCost   = sum $ take (fromIntegral fingersNeeded) $ iterate (*1.15) $ costs ^?! ix Cursor
  custom =
    [ finish 280 Cursor "Septillion fingers"
    , finish 250 Grandma "The Unbridling"
    , finish 200 Mine "Planetsplitters"
    , finish 200 Factory "Cyborg workforce"
    , finish 200 Bank "Way of the wallet"
    , finish 150 WizardTower "Dark formulas"
    , finish 100 Shipment "Chocolate monoliths"
    , finish 100 AlchemyLab "Aqua crustuale"
    , finish 100 Portal "Sanity dance"
    , finish 50 Antimatter "Big bang bake"
    , finish 50 Prism "Grainbow"
    ]

  buyBuilding =
    [( "+1 " ++ show x
     , effect (costs ^?! ix x) (buildingOwned x +~ 1)
     )
    | x <- [Cursor ..] ]

  buyUpgrades =
     [ (view upgradeName u, effect (view upgradeCost u) (upgradesBought <>~ [u]))
     | u <- view upgradesAvailable inp
     ]

  upgrades =
     [ "Sugar bosons"
     , "Grandmas' grandmas"

     , "Madeleines"
     , "Palmiers"
     , "Palets"
     , "Sablés"
     , "Gingerbread men"
     , "Gingerbread trees"
     , "Pure white chocolate cookies"
     , "Pure black chocolate cookies"
     , "Ladyfingers"

     , "Kitten overseers"
     ]

  costs = buildingCosts inp
  cps = computeCps inp st
  st = computeGameState inp

  effect cost f = (cost / (computeCps (f inp) (computeGameState (f inp)) - cps), cost)

  finish n b up = ("+" ++ show n' ++ " " ++ show b, effect cost f)
    where
    Just u = Map.lookup up upgradeMap
    n' = n - view (buildingOwned b) inp
    cost = view upgradeCost u + sum (take (fromIntegral n') (iterate (*1.15) (costs ^?! ix b)))
    f = (upgradesBought <>~ [u])
      . (achievementsEarned +~ 1)
      . (buildingOwned b .~ n)

computeCps :: GameInput -> GameState -> Double
computeCps inp st =
  view multiplier st *
  sum (Map.intersectionWith (\count cps -> fromIntegral count * cps)
         (view buildingsOwned inp)
         (computeBuildingCps st))

computeClickCookies :: GameInput -> GameState -> Double
computeClickCookies inp st =
    computeCps inp st * view mouseBonus st
  + view multiplier st * (view (buildingMult Cursor) st + view (buildingBonus Cursor) st)

loadMyInput :: IO GameInput
loadMyInput =
  do txt <- Text.readFile "input.yaml"
     either fail return (parseConfig =<< Config.parse txt)

main :: IO ()
main =
  do input <- loadMyInput
     putStrLn (payoff input)
     let st = computeGameState input
         cps = computeCps input st
     putStrLn $ "Buildings:\t"   ++ show (sum (view buildingsOwned input))
     putStrLn $ "Upgrades:\t"    ++ show (length (view upgradesBought input))
     putStrLn $ "Cookie/s:\t"    ++ prettyNumber LongSuffix cps
     putStrLn $ "Cookie/c:\t"    ++ prettyNumber LongSuffix (computeClickCookies input st)
     putStrLn $ "Reserve:\t"     ++ prettyNumber LongSuffix (7*6000*cps)
     putStrLn $ "Cookie/s x7:\t" ++ prettyNumber LongSuffix (7*cps)
     putStrLn $ "Lucky x7:\t"    ++ prettyNumber LongSuffix (7*900*cps)


parseConfig :: Config.Value -> Either String GameInput
parseConfig input =
  do achieve     <- section "achievements" input Config.number
     buildSect   <- section "buildings"    input Config.sections
     upgradeSect <- section "upgrades"     input Config.list
     shopSect    <- section "shop"         input Config.list

     buildings   <- traverse parseBuilding buildSect
     upgrades    <- traverse parseUpgrade  upgradeSect
     shop        <- traverse parseUpgrade  shopSect

     return GameInput
       { _achievementsEarned = achieve
       , _buildingsOwned     = Map.fromList buildings
       , _upgradesBought     = upgrades
       , _upgradesAvailable  = shop
       }

parseBuilding :: Config.Section -> Either String (Building, Integer)
parseBuilding (Config.Section b v) =
  case preview Config.number v of
    Just n | Just bld <- readMaybe (Text.unpack b) -> Right (bld, n)
    Nothing -> Left (Text.unpack b)

parseUpgrade :: Config.Value -> Either String Upgrade
parseUpgrade v =
  case preview Config.text v of
    Just txt -> 
      case Map.lookup (Text.unpack txt) upgradeMap of
        Just u -> Right u
        Nothing -> Left (Text.unpack txt)
    Nothing -> Left "bad upgrade"

section :: String -> Config.Value -> Traversal' Config.Value a -> Either String a
section k x l = maybe (Left k) Right (preview (Config.key (Text.pack k) . l) x)
{-# INLINE section #-}

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

upgradeMap :: Map String Upgrade
upgradeMap = Map.fromList [ (view upgradeName u, u) | u <- allUpgrades ]

gpoc :: Building -> Double -> GameInput -> GameState -> GameState
gpoc b bonus = \inp ->
  let gmas = views (buildingOwned b) fromIntegral inp
  in buildingBase Grandma +~ bonus * gmas

allUpgrades :: [Upgrade]
allUpgrades =
   [ Upgrade   0 "Reinforced Index Finger"        100.0e0  $ doubler Cursor
   , Upgrade   1 "Carpal tunnel prevention cream" 500.0e0  $ doubler Cursor
   , Upgrade   2 "Ambidextrous"                    10.0e3  $ doubler Cursor
   , Upgrade   3 "Thousand fingers"               100.0e3  $ cursorAdd 0.1
   , Upgrade   4 "Million fingers"                 10.0e6  $ cursorAdd 0.5
   , Upgrade   5 "Billion fingers"                100.0e6  $ cursorAdd 5
   , Upgrade   6 "Trillion fingers"                 1.0e9  $ cursorAdd 50
   , Upgrade  43 "Quadrillion fingers"             10.0e9  $ cursorAdd 500
   , Upgrade  82 "Quintillion fingers"             10.0e12 $ cursorAdd 5000
   , Upgrade 109 "Sextillion fingers"             100.0e12 $ cursorAdd 50000
   , Upgrade 188 "Septillion fingers"               1.0e15 $ cursorAdd 500000
   , Upgrade 189 "Octillion fingers"               10.0e15 $ cursorAdd 5000000

       ---- MICE
   , Upgrade  75 "Plastic mouse"     50.0e3  mouseAdd
   , Upgrade  76 "Iron mouse"         5.0e6  mouseAdd
   , Upgrade  77 "Titanium mouse"   500.0e6  mouseAdd
   , Upgrade  78 "Adamantium mouse"  50.0e9  mouseAdd
   , Upgrade 119 "Unobtainium mouse"  5.0e12 mouseAdd
   , Upgrade 190 "Eludium mouse"    500.0e12 mouseAdd
   , Upgrade 191 "Wishalloy mouse"   50.0e15 mouseAdd
   , Upgrade   0 "Fantasteel mouse"   5.0e18 mouseAdd
   , Upgrade   0 "Nevercrack mouse"  50.0e15 mouseAdd

   --    -- GRANDMAS
   , Upgrade   7 "Forwards from grandma"       1.0e3  $ doubler Grandma
   , Upgrade   8 "Steel-plated rolling pins"   5.0e3  $ doubler Grandma
   , Upgrade   9 "Lubricated dentures"        50.0e3  $ doubler Grandma
   , Upgrade  44 "Prune juice"                 5.0e6  $ doubler Grandma
   , Upgrade 110 "Double-thick glasses"      500.0e6  $ doubler Grandma
   , Upgrade 192 "Aging agents"               50.0e9  $ doubler Grandma
   , Upgrade 294 "Xtreme walkers"             50.0e12 $ doubler Grandma
   , Upgrade 307 "The Unbridling"             50.0e15 $ doubler Grandma

   , Upgrade  57 "Farmer grandmas"     55.0e3  $ grandmaType Farm         1
   , Upgrade  58 "Miner grandmas"     600.0e3  $ grandmaType Mine         2
   , Upgrade  59 "Worker grandmas"      6.5e6  $ grandmaType Factory      3
   , Upgrade 250 "Banker grandmas"     70.0e6  $ grandmaType Bank         4
   , Upgrade 251 "Priestess grandmas"   1.0e9  $ grandmaType Temple       5
   , Upgrade 252 "Witch grandmas"      16.5e9  $ grandmaType WizardTower  6
   , Upgrade  60 "Cosmic grandmas"    255.0e9  $ grandmaType Shipment     7
   , Upgrade  61 "Transmuted grandmas" 3.75e12 $ grandmaType AlchemyLab   8
   , Upgrade  62 "Altered grandmas"    50.0e12 $ grandmaType Portal       9
   , Upgrade  63 "Grandmas' grandmas" 700.0e12 $ grandmaType TimeMachine 10
   , Upgrade 103 "Antigrandmas"         8.5e15 $ grandmaType Antimatter  11
   , Upgrade 180 "Rainbow grandmas"   105.0e15 $ grandmaType Prism       12

   --    -- FARMS
   , Upgrade  10 "Cheap hoes"                    11.0e3  $ doubler Farm
   , Upgrade  11 "Fertilizer"                    55.0e3  $ doubler Farm
   , Upgrade  12 "Cookie trees"                 550.0e3  $ doubler Farm
   , Upgrade  45 "Genetically-modified cookies"  55.0e6  $ doubler Farm
   , Upgrade 111 "Gingerbread scarecrows"         5.5e9  $ doubler Farm
   , Upgrade 193 "Pulsar sprinklers"            550.0e9  $ doubler Farm
   , Upgrade 295 "Fudge fungus"                 550.0e12 $ doubler Farm
   , Upgrade 308 "Wheat triffids"               550.0e15 $ doubler Farm

   --    -- MINES
   , Upgrade  16 "Sugar gas"      120.0e3  $ doubler Mine
   , Upgrade  17 "Megadrill"      600.0e3  $ doubler Mine
   , Upgrade  18 "Ultradrill"       6.0e6  $ doubler Mine
   , Upgrade  47 "Ultimadrill"    600.0e6  $ doubler Mine
   , Upgrade 113 "H-bomb mining"   60.0e9  $ doubler Mine
   , Upgrade 195 "Coreforge"        6.0e12 $ doubler Mine
   , Upgrade 296 "Planetsplitters"  6.0e15 $ doubler Mine
   , Upgrade 309 "Canola oil wells" 6.0e18 $ doubler Mine

   --    -- FACTORIES
   , Upgrade  13 "Sturdier conveyor belts" 1.3e6  $ doubler Factory
   , Upgrade  14 "Child labor"             6.5e6  $ doubler Factory
   , Upgrade  15 "Sweatshop"              65.0e6  $ doubler Factory
   , Upgrade  46 "Radium reactors"         6.5e9  $ doubler Factory
   , Upgrade 112 "Recombobulators"       650.0e9  $ doubler Factory
   , Upgrade 194 "Deep-bake process"      65.0e12 $ doubler Factory
   , Upgrade 297 "Cyborg workforce"       65.0e15 $ doubler Factory
   , Upgrade 310 "78-hour days"           65.0e18 $ doubler Factory

   --    -- BANKS
   , Upgrade 232 "Taller tellers" 14e6 $ doubler Bank
   , Upgrade 233 "Scissor-resistant credit cards" 70e6 $ doubler Bank
   , Upgrade 234 "Acid-proof vaults" 700e6 $ doubler Bank
   , Upgrade 235 "Chocolate coins" 70e9 $ doubler Bank
   , Upgrade 236 "Exponential interest rates" 7e12 $ doubler Bank
   , Upgrade 237 "Financial zen" 700e12 $ doubler Bank
   , Upgrade 298 "Way of the wallet" 700e15 $ doubler Bank
   , Upgrade 311 "The stuff rationale" 700e18 $ doubler Bank

   --    -- TEMPLES
   , Upgrade 238 "Golden idols" 200e6 $ doubler Temple
   , Upgrade 239 "Sacrifices" 1e9 $ doubler Temple
   , Upgrade 240 "Delicious blessing" 10e9 $ doubler Temple
   , Upgrade 241 "Sun festival" 1e12 $ doubler Temple
   , Upgrade 242 "Enlarged pantheon" 100e12 $ doubler Temple
   , Upgrade 243 "Great Baker in the sky" 10e15 $ doubler Temple
   , Upgrade 299 "Creation myth" 10e18 $ doubler Temple
   , Upgrade 299 "Theocracy" 10e21 $ doubler Temple

   --    -- WIZARDS
   , Upgrade 244 "Pointier hats" 3.3e9 $ doubler WizardTower
   , Upgrade 245 "Beardlier beards" 16.5e9 $ doubler WizardTower
   , Upgrade 246 "Ancient grimoires" 165e9 $ doubler WizardTower
   , Upgrade 247 "Kitchen curses" 16.5e12 $ doubler WizardTower
   , Upgrade 248 "School of sorcery" 1.65e15 $ doubler WizardTower
   , Upgrade 249 "Dark formulas" 165e15 $ doubler WizardTower
   , Upgrade 300 "Cookiemancy" 165e18 $ doubler WizardTower
   , Upgrade 313 "Rabbit trick" 165e21 $ doubler WizardTower

   --    -- SHIPMENTS
   , Upgrade  19  "Vanilla nebulae" 51e9 $ doubler Shipment
   , Upgrade  20 "Wormholes" 255e9 $ doubler Shipment
   , Upgrade  21 "Frequent flier" 2.55e12 $ doubler Shipment
   , Upgrade  48 "Warp drive" 255e12 $ doubler Shipment
   , Upgrade 114 "Chocolate monoliths" 25.5e15 $ doubler Shipment
   , Upgrade 196 "Generation Ship" 2.55e18 $ doubler Shipment
   , Upgrade 301 "Dyson sphere" 2.55e21 $ doubler Shipment
   , Upgrade 314 "The final frontier" 2.55e24 $ doubler Shipment

   --    -- ALCHEMY
   , Upgrade  22 "Antimony" 750e9 $ doubler AlchemyLab
   , Upgrade  23 "Essence of dough" 3.75e12 $ doubler AlchemyLab
   , Upgrade  24 "True chocolate" 37.5e12 $ doubler AlchemyLab
   , Upgrade  49 "Ambrosia" 3.75e15 $ doubler AlchemyLab
   , Upgrade 115 "Aqua crustuale" 375e15 $ doubler AlchemyLab
   , Upgrade 197 "Origin crucible" 3.75e18 $ doubler AlchemyLab
   , Upgrade 302 "Theory of atomic fluidity" 37.5e21 $ doubler AlchemyLab
   , Upgrade 315 "Beige goo" 37.5e24 $ doubler AlchemyLab

   --    -- PORTAL
   , Upgrade  25 "Ancient tablet" 10e12 $ doubler Portal
   , Upgrade  26 "Insane oatling workers" 50e12 $ doubler Portal
   , Upgrade  27 "Soul bond" 500e12 $ doubler Portal
   , Upgrade  50 "Sanity dance" 50e15 $ doubler Portal
   , Upgrade 116 "Brane transplant" 5e18 $ doubler Portal
   , Upgrade 198 "Deity-sized portals" 500e18 $ doubler Portal
   , Upgrade 303 "End of back-up plan" 500e21 $ doubler Portal
   , Upgrade 316 "Maddening chants" 500e24 $ doubler Portal

   -- TIME MACHINE
   , Upgrade  28 "Flux capacitors" 140e12 $ doubler TimeMachine
   , Upgrade  29 "Time paradox resolver" 700e12 $ doubler TimeMachine
   , Upgrade  30 "Quantum conundrum" 7e15 $ doubler TimeMachine
   , Upgrade  51 "Causality enforcer" 700e15 $ doubler TimeMachine
   , Upgrade 117 "Yestermorrow comparators" 70e18 $ doubler TimeMachine
   , Upgrade 199 "Far future enactment" 7e21 $ doubler TimeMachine
   , Upgrade 304 "Great loop hypothesis" 7e24 $ doubler TimeMachine
   , Upgrade 317 "Cookietopian moments of maybe" 7e27 $ doubler TimeMachine

   -- ANTIMATTER CONDENSER
   , Upgrade 99 "Sugar bosons" 1.7e15 $ doubler Antimatter
   , Upgrade 100 "String theory" 8.5e15 $ doubler Antimatter
   , Upgrade 101 "Large macaron collider" 85e15 $ doubler Antimatter
   , Upgrade 102 "Big bang bake" 8.5e18 $ doubler Antimatter
   , Upgrade 118 "Reverse cyclotrons" 850e18 $ doubler Antimatter
   , Upgrade 200 "Nanocosmics" 85e21 $ doubler Antimatter
   , Upgrade 305 "The Pulse" 85e24 $ doubler Antimatter
   , Upgrade 318 "Some other super-tiny fundamental particle? Probably?" 85e27 $ doubler Antimatter

   -- PRISM
   , Upgrade 175 "Gem polish"          21e15 $ doubler Prism
   , Upgrade 176 "9th color"          105e15 $ doubler Prism
   , Upgrade 177 "Chocolate light"   1.05e18 $ doubler Prism
   , Upgrade 178 "Grainbow"           105e18 $ doubler Prism
   , Upgrade 179 "Pure cosmic light" 10.5e21 $ doubler Prism
   , Upgrade 201 "Glow-in-the-dark"  1.05e24 $ doubler Prism
   , Upgrade 306 "Lux sanctorum"     1.05e27 $ doubler Prism
   , Upgrade 319 "Reverse shadows"   1.05e30 $ doubler Prism

   --    -- KITTENS
   , Upgrade 31  "Kitten helpers"       9.0e6  $ kittenBonus 10
   , Upgrade 32  "Kitten workers"       9.0e9  $ kittenBonus 12.5
   , Upgrade 54  "Kitten engineers"    90.0e12 $ kittenBonus 15
   , Upgrade 108 "Kitten overseers"    90.0e15 $ kittenBonus 17.5
   , Upgrade 187 "Kitten managers"    900.0e18 $ kittenBonus 20
   , Upgrade 320 "Kitten accountants" 900.0e21 $ kittenBonus 20
   , Upgrade 321 "Kitten specialists" 900.0e24 $ kittenBonus 20
   , Upgrade 322 "Kitten experts"     900.0e27 $ kittenBonus 20
   -- , Upgrade 291 "Kitten angels" 9000HC

   --    -- COOKIES
   , Upgrade  33 "Plain cookies"                      999999    $ cookieBonus 1
   , Upgrade  34 "Sugar cookies"                           5e6  $ cookieBonus 1
   , Upgrade  35 "Oatmeal raisin cookies"                 10e6  $ cookieBonus 1
   , Upgrade  36 "Peanut butter cookies"                  50e6  $ cookieBonus 1
   , Upgrade  37 "Coconut cookies"                       100e6  $ cookieBonus 1
   , Upgrade  38 "White chocolate cookies"               500e6  $ cookieBonus 2
   , Upgrade  39 "Macadamia nut cookies"                   1e9  $ cookieBonus 2
   , Upgrade  40 "Double-chip cookies"                     5e9  $ cookieBonus 2
   , Upgrade  41 "While chocolate macadamia nut cookies"  10e9  $ cookieBonus 2
   , Upgrade  42 "All-chocolate cookies"                  50e9  $ cookieBonus 2
   , Upgrade  55 "Dark chocolate-coated cookies"         100e9  $ cookieBonus 4
   , Upgrade  56 "White chocolate-coated cookies"        100e9  $ cookieBonus 4
   , Upgrade  80 "Eclipse cookies"                       500e9  $ cookieBonus 2
   , Upgrade  81 "Zebra cookies"                           1e12 $ cookieBonus 2
   , Upgrade  88 "Snickerdoodles"                          5e12 $ cookieBonus 2
   , Upgrade  89 "Stroopwafels"                           10e12 $ cookieBonus 2
   , Upgrade  90 "Macaroons"                              50e12 $ cookieBonus 2
   , Upgrade  92 "Empire biscuits"                       100e12 $ cookieBonus 2
   , Upgrade 104 "Madeleines"                            500e12 $ cookieBonus 2
   , Upgrade 105 "Palmiers"                              500e12 $ cookieBonus 2
   , Upgrade 106 "Palets"                                  1e15 $ cookieBonus 2
   , Upgrade 107 "Sablés"                                  1e15 $ cookieBonus 2
   , Upgrade 120 "Caramoas"                               10e15 $ cookieBonus 3
   , Upgrade 121 "Sagalongs"                              10e15 $ cookieBonus 3
   , Upgrade 122 "Shortfoils"                             10e15 $ cookieBonus 3
   , Upgrade 123 "Win mints"                              10e15 $ cookieBonus 3
   , Upgrade 150 "Gingerbread men"                        10e15 $ cookieBonus 2
   , Upgrade 151 "Gingerbread trees"                      10e15 $ cookieBonus 2
   , Upgrade 256 "Pure black chocolate cookies"           50e15 $ cookieBonus 4
   , Upgrade 257 "Pure white chocolate cookies"           50e15 $ cookieBonus 4
   , Upgrade 258 "Ladyfingers"                           100e15 $ cookieBonus 3
   , Upgrade 259 "Tuiles"                                500e15 $ cookieBonus 3
   , Upgrade 260 "Chocolate-stuffed biscuits"              1e18 $ cookieBonus 3
   , Upgrade 261 "Checker cookies"                          5e18 $ cookieBonus 3
   , Upgrade 262 "Butter cookies"                         10e18 $ cookieBonus 3
   , Upgrade 263 "Cream cookies"                          50e18 $ cookieBonus 3

   , Upgrade 52 "Lucky day"        777777777 $ const id
   , Upgrade 53 "Serendipity"    77777777777 $ const id
   , Upgrade 86 "Get lucky"   77777777777777 $ const id

   , Upgrade 64 "Bingo center/Research facility" 1e15   $ \_ -> buildingMult Grandma *~ 4
   , Upgrade 65 "Specialized chocolate chips"    100e9  $ cookieBonus 1
   , Upgrade 66 "Designer cocoa beans"           200e9  $ cookieBonus 2
   , Upgrade 67 "Ritual rolling pins"            400e9  $ doubler Grandma
   , Upgrade 68 "Underworld ovens"               800e9  $ cookieBonus 3
   , Upgrade 69 "One mind"                       1.6e9  $ gpoc Grandma 0.02
   , Upgrade 70 "Exotic nuts"                    3.2e9  $ cookieBonus 4
   , Upgrade 71 "Communal brainsweep"            6.4e9  $ gpoc Grandma 0.02
   , Upgrade 72 "Arcane sugar"                   12.8e9 $ cookieBonus 5
   , Upgrade 72 "Elder pact"                     25.6e9 $ gpoc Portal 0.05
   ]
