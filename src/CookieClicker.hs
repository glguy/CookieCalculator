{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language ForeignFunctionInterface #-}
{-# Language RankNTypes #-}

module Main (main) where

import GameInput
import Building
import SaveFormat
import SourceData

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Lens hiding (ReifiedPrism(..), prism)
import Data.List
import Data.Time
import Foreign.C.Types (CDouble(..))
import Numeric
import Spreadsheet
import Spreadsheet.Renderer
import Spreadsheet.Sorting
import qualified Data.Text as Text

initialBuildingStat :: Double -> BuildingStat
initialBuildingStat base = BuildingStat
  { _bldgBase = base
  , _bldgMult = 1
  , _bldgBonus = 0
  , _bldgFree = 0
  }

initialGameState :: GameState
initialGameState = GameState
  { _buildingStats           = initialBuildingStat <$> baseCps
  , _multiplier              = 1
  , _eggMultiplier           = 1
  , _mouseBonus              = 0
  , _mouseMultiplier         = 1
  , _prestigeMultiplier      = 0
  , _bonusCps                = 0
  , _buildingCostMultiplier  = 1
  , _upgradeCostMultiplier   = 1
  , _milkMultiplier          = 1
  , _milkFactors             = []
  , _wrinklerMultiplier      = 1.1
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

upgradeEffect :: Upgrade -> Effect
upgradeEffect = views upgradeName effectByName

effectByName :: Text -> Effect
effectByName n =
  Map.findWithDefault
    (error ("Unknown effect: " ++ Text.unpack n))
    n
    upgradeEffects

computeGameState :: GameInput -> GameState
computeGameState input
  = effectByName (view dragonAura1 input) input
  $ effectByName (view dragonAura2 input) input
  $ foldl'
      (\acc u -> upgradeEffect u input acc)
      initialGameState
      (view upgradesBought input)

type Effect = GameInput -> GameState -> GameState

mouseAdd :: Effect
mouseAdd = \_ -> mouseBonus +~ 0.01

kittenBonus :: Double -> Effect
kittenBonus pct = \_ -> milkFactors %~ (pct/100:)

cookieBonus :: Double -> Effect
cookieBonus pct = \_ -> multiplier *~ (1+pct/100)

cursorAdd :: Double -> Effect
cursorAdd bonus = \inp ->
  let count = sum (Map.delete Cursor (view buildingsOwned inp))
  in buildingBonus Cursor +~ bonus * fromIntegral count

grandmaType :: Building -> Int -> Effect
grandmaType building count = \inp ->
    let bonus = fromIntegral (inp^.buildingOwned Grandma)
              / fromIntegral count
    in (buildingMult building *~ (1 + 0.01 * bonus))
     . (buildingMult Grandma  *~ 2)

doubler :: Building -> GameInput -> GameState -> GameState
doubler k _ = buildingMult k *~ 2

computeBuildingStatCps :: BuildingStat -> Double
computeBuildingStatCps stat = stat^.bldgBonus + stat^.bldgMult * stat^.bldgBase

computeBuildingCps :: GameState -> Map Building Double
computeBuildingCps st = computeBuildingStatCps <$> view buildingStats st

leftJoinWith' :: Ord k => (a -> b -> a) -> Map k a -> Map k b -> Map k a
leftJoinWith' f = Map.mergeWithKey (\_ x y -> Just $! f x y) id (\_ -> Map.empty)

buildingCosts :: GameInput -> GameState -> Map Building Double
buildingCosts inp st
  = fmap (* view buildingCostMultiplier st)
  $ leftJoinWith'
      (\base n -> base * 1.15 ** fromIntegral n)
      initialCosts
      owned'
  where
  owned' = leftJoinWith' (-)
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

  custom =
    [ finishA 300 Temple
    , finishA 300 WizardTower
    , finishA 400 Cursor
    , finish True 250 Shipment "The final frontier"
    , finish True 250 AlchemyLab "Beige goo"
    , finish True 250 Portal "Maddening chants"
    , finish True 200 TimeMachine "Great loop hypothesis"
    , finish True 200 Antimatter "The Pulse"
    , finish True 200 Prism "Lux sanctorum"
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
       , upgradesBought %~ cons u
       )
     | u <- view upgradesAvailable inp
     ]

  costs = buildingCosts inp st
  cps   = computeCps inp st

  effect f = computeCps (f inp) (computeGameState (f inp)) - cps

  finish :: Bool -> Int -> Building -> Text -> (String, Double, GameInput -> GameInput)
  finish a n b up = ("+" ++ show n' ++ " " ++ show b, cost, f)
    where
    u = Map.findWithDefault (error ("Unknown upgrade: " ++ Text.unpack up))
               up
               upgradeByName
    n' = n - view (buildingOwned b) inp
    cost = view upgradeCost u + sum (take (fromIntegral n') (iterate (*1.15) (costs ^?! ix b)))
    f = (upgradesBought %~ cons u)
      . (achievementsEarned %~ cons (fakeAchievement a))
      . (buildingOwned b .~ n)

  finishA n b = ("+" ++ show n' ++ " " ++ show b, cost, f)
    where
    n' = n - view (buildingOwned b) inp
    cost = sum (take (fromIntegral n') (iterate (*1.15) (costs ^?! ix b)))
    f = (achievementsEarned %~ cons (fakeAchievement True))
      . (buildingOwned b .~ n)

  fakeAchievement True  = Achievement "fake" "normal"
  fakeAchievement False = Achievement "fake" "shadow"

computeMultiplier :: GameInput -> GameState -> Double
computeMultiplier inp st
  = view multiplier st
  * milkFactor
  * view eggMultiplier st
  * prestigeFactor

  where
  milkFactor = product [ 1 + milk * x | x <- view milkFactors st ]
  milk = computeMilk inp * view milkMultiplier st

  prestigeFactor = 1 + view prestigeMultiplier st
                     * view prestigeLevel inp / 100

computeMilk :: GameInput -> Double
computeMilk input = fromIntegral n / 25
  where
  n = lengthOf (achievementsEarned . folded . filtered (views achievementPool (/= "shadow"))) input

computeCps :: GameInput -> GameState -> Double
computeCps inp st =
  computeMultiplier inp st *
  (view bonusCps st +
  sum (Map.intersectionWith (\count cps -> fromIntegral count * cps)
         (view buildingsOwned inp)
         (computeBuildingCps st))
  )

computeClickCookies :: GameInput -> GameState -> Double
computeClickCookies inp st = view mouseMultiplier st * cpc
  where
  cpc = computeCps inp st * view mouseBonus st
      + view (buildingBonus Cursor) st
      + view (buildingMult Cursor) st

  mult = computeMultiplier inp st

loadMyInput :: IO GameInput
loadMyInput =
  do now <- getCurrentTime
     saveFileToGameInput now <$> loadMySave

main :: IO ()
main = report =<< loadMyInput

report :: GameInput -> IO ()
report input =
  do let st = computeGameState input
     putStrLn (payoff input st)
     let cps = computeCps input st
     putStrLn $ "Buildings:\t"   ++ show (sum (view buildingsOwned input))
     putStrLn $ "Cookies:\t"     ++ prettyNumber LongSuffix (view cookiesBanked input)
     putStrLn $ "Munched:\t"     ++ prettyNumber LongSuffix (computeMunched input st)
     putStrLn $ "Cookie/s:\t"    ++ prettyNumber LongSuffix cps
     putStrLn $ "Cookie/c:\t"    ++ prettyNumber LongSuffix (computeClickCookies input st)
     putStrLn $ "Reserve:\t"     ++ prettyNumber LongSuffix (7*6000*cps)
     putStrLn $ "Cookie/s x7:\t" ++ prettyNumber LongSuffix (7*cps)
     putStrLn $ "Lucky x7:\t"    ++ prettyNumber LongSuffix (7*900*cps)

computeMunched :: GameInput -> GameState -> Double
computeMunched input st = view wrinklerMultiplier st * view cookiesMunched input

data SuffixLength = LongSuffix | ShortSuffix

prettyNumber :: SuffixLength -> Double -> String
prettyNumber s n
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

{-
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
-}

gpoc :: Building -> Double -> Effect
gpoc b bonus = \inp ->
  let gmas = views (buildingOwned b) fromIntegral inp
  in buildingBase Grandma +~ bonus * gmas

upgradeEffects :: Map Text Effect
upgradeEffects = Map.fromList $
   [ (name, doubler b) | b <- [Grandma .. ], name <- buildingTieredUpgrades b ]
   ++
   [ ("Reinforced index finger"        , doubler Cursor)
   , ("Carpal tunnel prevention cream" , doubler Cursor)
   , ("Ambidextrous"                   , doubler Cursor)
   , ("Thousand fingers"               , cursorAdd 1.0e-1)
   , ("Million fingers"                , cursorAdd 5.0e-1)
   , ("Billion fingers"                , cursorAdd 5.0e+0)
   , ("Trillion fingers"               , cursorAdd 5.0e+1)
   , ("Quadrillion fingers"            , cursorAdd 5.0e+2)
   , ("Quintillion fingers"            , cursorAdd 5.0e+3)
   , ("Sextillion fingers"             , cursorAdd 5.0e+4)
   , ("Septillion fingers"             , cursorAdd 5.0e+5)
   , ("Octillion fingers"              , cursorAdd 5.0e+6)

       ---- MICE
   , ("Plastic mouse"    , mouseAdd)
   , ("Iron mouse"       , mouseAdd)
   , ("Titanium mouse"   , mouseAdd)
   , ("Adamantium mouse" , mouseAdd)
   , ("Unobtainium mouse", mouseAdd)
   , ("Eludium mouse"    , mouseAdd)
   , ("Wishalloy mouse"  , mouseAdd)
   , ("Fantasteel mouse" , mouseAdd)
   , ("Nevercrack mouse" , mouseAdd)

   , ("Farmer grandmas"    , grandmaType Farm         1)
   , ("Miner grandmas"     , grandmaType Mine         2)
   , ("Worker grandmas"    , grandmaType Factory      3)
   , ("Banker grandmas"    , grandmaType Bank         4)
   , ("Priestess grandmas" , grandmaType Temple       5)
   , ("Witch grandmas"     , grandmaType WizardTower  6)
   , ("Cosmic grandmas"    , grandmaType Shipment     7)
   , ("Transmuted grandmas", grandmaType AlchemyLab   8)
   , ("Altered grandmas"   , grandmaType Portal       9)
   , ("Grandmas' grandmas" , grandmaType TimeMachine 10)
   , ("Antigrandmas"       , grandmaType Antimatter  11)
   , ("Rainbow grandmas"   , grandmaType Prism       12)

   --    -- KITTENS
   , ("Kitten helpers"    , kittenBonus 10)
   , ("Kitten workers"    , kittenBonus 12.5)
   , ("Kitten engineers"  , kittenBonus 15)
   , ("Kitten overseers"  , kittenBonus 17.5)
   , ("Kitten managers"   , kittenBonus 20)
   , ("Kitten accountants", kittenBonus 20)
   , ("Kitten specialists", kittenBonus 20)
   , ("Kitten experts"    , kittenBonus 20)
   , ("Kitten angels"     , kittenBonus 10)

   --    -- COOKIES
   , ("Plain cookies"                        , cookieBonus 1)
   , ("Sugar cookies"                        , cookieBonus 1)
   , ("Oatmeal raisin cookies"               , cookieBonus 1)
   , ("Peanut butter cookies"                , cookieBonus 1)
   , ("Coconut cookies"                      , cookieBonus 1)
   , ("White chocolate cookies"              , cookieBonus 2)
   , ("Macadamia nut cookies"                , cookieBonus 2)
   , ("Double-chip cookies"                  , cookieBonus 2)
   , ("White chocolate macadamia nut cookies", cookieBonus 2)
   , ("All-chocolate cookies"                , cookieBonus 2)
   , ("Dark chocolate-coated cookies"        , cookieBonus 4)
   , ("White chocolate-coated cookies"       , cookieBonus 4)
   , ("Eclipse cookies"                      , cookieBonus 2)
   , ("Zebra cookies"                        , cookieBonus 2)
   , ("Snickerdoodles"                       , cookieBonus 2)
   , ("Stroopwafels"                         , cookieBonus 2)
   , ("Macaroons"                            , cookieBonus 2)
   , ("Empire biscuits"                      , cookieBonus 2)
   , ("Madeleines"                           , cookieBonus 2)
   , ("Palmiers"                             , cookieBonus 2)
   , ("Palets"                               , cookieBonus 2)
   , ("Sablés"                               , cookieBonus 2)
   , ("Caramoas"                             , cookieBonus 3)
   , ("Sagalongs"                            , cookieBonus 3)
   , ("Shortfoils"                           , cookieBonus 3)
   , ("Win mints"                            , cookieBonus 3)
   , ("Gingerbread men"                      , cookieBonus 2)
   , ("Gingerbread trees"                    , cookieBonus 2)
   , ("Pure black chocolate cookies"         , cookieBonus 4)
   , ("Pure white chocolate cookies"         , cookieBonus 4)
   , ("Ladyfingers"                          , cookieBonus 3)
   , ("Tuiles"                               , cookieBonus 3)
   , ("Chocolate-stuffed biscuits"           , cookieBonus 3)
   , ("Checker cookies"                      , cookieBonus 3)
   , ("Butter cookies"                       , cookieBonus 3)
   , ("Cream cookies"                        , cookieBonus 3)
   , ("Gingersnaps"                          , cookieBonus 4)
   , ("Cinnamon cookies"                     , cookieBonus 4)
   , ("Vanity cookies"                       , cookieBonus 4)
   , ("Cigars"                               , cookieBonus 4)
   , ("Pinwheel cookies"                     , cookieBonus 4)
   , ("Fudge squares"                        , cookieBonus 4)
   , ("Butter horseshoes"                    , cookieBonus 4)
   , ("Shortbread biscuits"                  , cookieBonus 4)
   , ("Butter pucks"                         , cookieBonus 4)
   , ("Butter knots"                         , cookieBonus 4)
   , ("Caramel cookies"                      , cookieBonus 4)
   , ("Millionaires' shortbreads"            , cookieBonus 4)
   , ("Butter slabs"                         , cookieBonus 4)
   , ("Butter swirls"                        , cookieBonus 4)

   , ("Milk chocolate butter biscuit"        , cookieBonus 10)
   , ("Dark chocolate butter biscuit"        , cookieBonus 10)
   , ("White chocolate butter biscuit"       , cookieBonus 10)
   , ("Ruby chocolate butter biscuit"        , cookieBonus 10)

   , ("Dragon cookie", cookieBonus 5)

   , ("Digits"       , cookieBonus 2)
   , ("Jaffa cakes"  , cookieBonus 2)
   , ("Loreols"      , cookieBonus 2)
   , ("Fig gluttons" , cookieBonus 2)
   , ("Grease's cups", cookieBonus 2)
   , ("Shortfolios"  , cookieBonus 3)

   , ("British tea biscuits"                                 , cookieBonus 2)
   , ("Chocolate british tea biscuits"                       , cookieBonus 2)
   , ("Round british tea biscuits"                           , cookieBonus 2)
   , ("Round chocolate british tea biscuits"                 , cookieBonus 2)
   , ("Round british tea biscuits with heart motif"          , cookieBonus 2)
   , ("Round chocolate british tea biscuits with heart motif", cookieBonus 2)

   , ("Rose macarons"     , cookieBonus 3)
   , ("Lemon macarons"    , cookieBonus 3)
   , ("Chocolate macarons", cookieBonus 3)
   , ("Pistachio macarons", cookieBonus 3)
   , ("Hazelnut macarons" , cookieBonus 3)
   , ("Violet macarons"   , cookieBonus 3)
   , ("Caramel macarons"  , cookieBonus 3)
   , ("Licorice macarons" , cookieBonus 3)

   , ("Lucky day"  , noEffect)
   , ("Serendipity", noEffect)
   , ("Get lucky"  , noEffect)

   , ("Bingo center/Research facility", \_ -> buildingMult Grandma *~ 4)
   , ("Specialized chocolate chips"   , cookieBonus 1)
   , ("Designer cocoa beans"          , cookieBonus 2)
   , ("Ritual rolling pins"           , doubler Grandma)
   , ("Underworld ovens"              , cookieBonus 3)
   , ("One mind"                      , gpoc Grandma 0.02)
   , ("Exotic nuts"                   , cookieBonus 4)
   , ("Communal brainsweep"           , gpoc Grandma 0.02)
   , ("Arcane sugar"                  , cookieBonus 5)
   , ("Elder Pact"                    , gpoc Portal 0.05)
   , ("Sacrificial rolling pins"      , noEffect)

   , ("Salmon roe"   , eggBonus)
   , ("Ant larva"    , eggBonus)
   , ("Cassowary egg", eggBonus)
   , ("Duck egg"     , eggBonus)
   , ("Turkey egg"   , eggBonus)
   , ("Turtle egg"   , eggBonus)
   , ("Quail egg"    , eggBonus)
   , ("Robin egg"    , eggBonus)
   , ("Ostrich egg"  , eggBonus)
   , ("Shark egg"    , eggBonus)
   , ("Chicken egg"  , eggBonus)
   , ("Frogspawn"    , eggBonus)
   , ("Century egg"  , addEggTimeBonus)
   , ("Cookie egg"   , \_ -> mouseMultiplier *~ 1.1)
   , ("Wrinklerspawn", \_ -> wrinklerMultiplier *~ 1.05)

   , ("Faberge egg", \_ -> buildingCostMultiplier *~ 0.99)
   , ("\"egg\"", \_ -> bonusCps +~ 9)

   , ("A crumbly egg", noEffect)

   , ("Pure heart biscuits"   , cookieBonus 2)
   , ("Ardent heart biscuits" , cookieBonus 2)
   , ("Sour heart biscuits"   , cookieBonus 2)
   , ("Weeping heart biscuits", cookieBonus 2)
   , ("Golden heart biscuits" , cookieBonus 2)
   , ("Eternal heart biscuits", cookieBonus 2)

   , ("Christmas tree biscuits", cookieBonus 2)
   , ("Snowflake biscuits"     , cookieBonus 2)
   , ("Snowman biscuits"       , cookieBonus 2)
   , ("Holly biscuits"         , cookieBonus 2)
   , ("Candy cane biscuits"    , cookieBonus 2)
   , ("Bell biscuits"          , cookieBonus 2)
   , ("Present biscuits"       , cookieBonus 2)

   , ("Heavenly chip secret"  , prestigeBonus 5)
   , ("Heavenly cookie stand" , prestigeBonus 20)
   , ("Heavenly bakery"       , prestigeBonus 25)
   , ("Heavenly confectionery", prestigeBonus 25)
   , ("Heavenly key"          , prestigeBonus 25)

   , ("A festive hat"             , noEffect)
   , ("Increased merriness"       , cookieBonus 15)
   , ("Improved jolliness"        , cookieBonus 15)
   , ("A lump of coal"            , cookieBonus 1)
   , ("An itchy sweater"          , cookieBonus 1)
   , ("Reindeer baking grounds"   , noEffect)
   , ("Weighted sleds"            , noEffect)
   , ("Ho ho ho-flavored frosting", noEffect)
   , ("Season savings"            , \_ -> buildingCostMultiplier *~ 0.99)
   , ("Toy workshop"              , \_ -> upgradeCostMultiplier *~ 0.95)
   , ("Naughty list"              , doubler Grandma)
   , ("Santa's bottomless bag"    , noEffect) -- drops
   , ("Santa's helpers"           , \_ -> mouseMultiplier *~ 1.1)
   , ("Santa's legacy"            , cookieBonus (15*3))
   , ("Santa's milk and cookies"  , \_ -> milkMultiplier *~ 1.05)
   , ("Santa's dominion"          , \inp -> cookieBonus 20 inp
                                         . (buildingCostMultiplier *~ 0.99)
                                         . (upgradeCostMultiplier *~ 0.98))

   , ("Skull cookies"  , cookieBonus 2)
   , ("Ghost cookies"  , cookieBonus 2)
   , ("Bat cookies"    , cookieBonus 2)
   , ("Slime cookies"  , cookieBonus 2)
   , ("Pumpkin cookies", cookieBonus 2)
   , ("Eyeball cookies", cookieBonus 2)
   , ("Spider cookies" , cookieBonus 2)

   , ("Future almanacs"             , synergy Farm TimeMachine)
   , ("Seismic magic"               , synergy Mine WizardTower)
   , ("Quantum electronics"         , synergy Factory Antimatter)
   , ("Contracts from beyond"       , synergy Bank Portal)
   , ("Paganism"                    , synergy Temple Portal)
   , ("Arcane knowledge"            , synergy WizardTower AlchemyLab)
   , ("Fossil fuels"                , synergy Mine Shipment)
   , ("Primordial ores"             , synergy Mine AlchemyLab)
   , ("Infernal crops"              , synergy Farm Portal)
   , ("Relativistic parsec-skipping", synergy Shipment TimeMachine)
   , ("Extra physics funding"       , synergy Bank Antimatter)
   , ("Light magic"                 , synergy WizardTower Prism)

   -- , ("Rain prayer" 0 $ synergy Temple Farm
   -- , ("Asteroid mining" 0 $ synergy Mine Shipment
   -- , ("Temporal overclocking" 0 $ synergy Factory TimeMachine
   -- , ("Printing press" 0 $ synergy Bank Factory
   -- , ("God particle" 0 $ synergy Antimatter Temple
   -- , ("Magical botany" 0 $ synergy Farm WizardTower
   -- , ("Shipyards" 0 $ synergy Factory Shipment
   -- , ("Gold fund" 0 $ synergy Bank AlchemyLab
   -- , ("Abysmal glimmer" 0 $ synergy Portal Prism

   , ("Revoke Elder Covenant"      , noEffect)
   , ("Persistent memory"          , noEffect)
   , ("Weighted sleighs"           , noEffect)
   , ("Season switcher"            , noEffect)
   , ("Bunny biscuit"              , noEffect)
   , ("Tin of british tea biscuits", noEffect)
   , ("Box of macarons"            , noEffect)
   , ("Box of brand biscuits"      , noEffect)
   , ("Permanent upgrade slot I"   , noEffect)
   , ("Permanent upgrade slot II"  , noEffect)
   , ("Permanent upgrade slot III"  , noEffect)
   , ("Permanent upgrade slot IIIV"  , noEffect)
   , ("Permanent upgrade slot V"  , noEffect)
   , ("Angels"                     , noEffect)
   , ("Archangels"                 , noEffect)
   , ("Virtues"                    , noEffect)
   , ("Dominions"                  , noEffect)
   , ("Twin Gates of Transcendence", noEffect)
   , ("Heavenly luck"              , noEffect)
   , ("Lasting fortune"            , noEffect)

   , ("Starter kit"    , \_ -> buildingFree Cursor  +~ 10)
   , ("Starter kitchen", \_ -> buildingFree Grandma +~  5)

   , ("How to bake your dragon", noEffect)
   , ("Tin of butter cookies"  , noEffect)
   , ("Golden switch"          , noEffect) -- enables the switch
   , ("Classic dairy selection", noEffect)
   , ("Belphegor"              , noEffect)
   , ("Mammon"                 , noEffect)
   , ("Abaddon"                , noEffect)
   , ("Satan"                  , noEffect)
   , ("Legacy"                 , noEffect)
   , ("Synergies Vol. I"       , noEffect)
   , ("Synergies Vol. II"      , noEffect)
   , ("Elder Pledge"           , noEffect)
   , ("Elder Covenant"         , noEffect)
   , ("Festive biscuit"        , noEffect)
   , ("Ghostly biscuit"        , noEffect)
   , ("Lovesick biscuit"       , noEffect)
   , ("Fool's biscuit"         , noEffect)
   , ("Golden switch [off]"    , cookieBonus 50)
   , ("Golden switch [on]"     , noEffect)
   , ("Milk selector"          , noEffect)
   , ("Golden goose egg"       , noEffect)
   , ("Chocolate egg"          , noEffect)
   , ("Decisive fate"          , noEffect)

   -- Dragon Auras
   , ("No aura"         , noEffect)
   , ("Radiant Appetite", cookieBonus 100)
   , ("Dragonflight"    , noEffect) -- effect not modeled
   , ("Mind Over Matter", noEffect) -- 0.75 multiplier to random drops
   ]

buildingTieredUpgrades :: Building -> [Text]
buildingTieredUpgrades b =
  case b of
    Cursor -> []

    Grandma -> [ "Forwards from grandma", "Steel-plated rolling pins",
                 "Lubricated dentures", "Prune juice", "Double-thick glasses",
                 "Aging agents", "Xtreme walkers", "The Unbridling"]

    Farm -> [ "Cheap hoes", "Fertilizer", "Cookie trees",
              "Genetically-modified cookies", "Gingerbread scarecrows",
              "Pulsar sprinklers", "Fudge fungus", "Wheat triffids"]

    Mine -> [ "Sugar gas", "Megadrill", "Ultradrill", "Ultimadrill",
              "H-bomb mining", "Coreforge", "Planetsplitters",
              "Canola oil wells"]

    Factory -> [ "Sturdier conveyor belts", "Child labor", "Sweatshop",
                 "Radium reactors", "Recombobulators", "Deep-bake process",
                 "Cyborg workforce", "78-hour days" ]

    Bank -> [ "Taller tellers", "Scissor-resistant credit cards",
              "Acid-proof vaults", "Chocolate coins",
              "Exponential interest rates", "Financial zen",
              "Way of the wallet", "The stuff rationale" ]

    Temple -> [ "Golden idols", "Sacrifices", "Delicious blessing",
                "Sun festival", "Enlarged pantheon", "Great Baker in the sky",
                "Creation myth", "Theocracy" ]

    WizardTower -> [ "Pointier hats", "Beardlier beards", "Ancient grimoires",
                     "Kitchen curses", "School of sorcery", "Dark formulas",
                     "Cookiemancy", "Rabbit trick" ]

    Shipment -> [ "Vanilla nebulae", "Wormholes", "Frequent flyer",
                  "Warp drive", "Chocolate monoliths", "Generation ship",
                  "Dyson sphere", "The final frontier" ]

    AlchemyLab -> [ "Antimony", "Essence of dough", "True chocolate",
                    "Ambrosia", "Aqua crustulae", "Origin crucible",
                    "Theory of atomic fluidity", "Beige goo" ]

    Portal -> [ "Ancient tablet", "Insane oatling workers", "Soul bond",
                "Sanity dance", "Brane transplant", "Deity-sized portals",
                "End of times back-up plan", "Maddening chants" ]

    TimeMachine -> [ "Flux capacitors", "Time paradox resolver",
                     "Quantum conundrum", "Causality enforcer",
                     "Yestermorrow comparators", "Far future enactment",
                     "Great loop hypothesis", "Cookietopian moments of maybe" ]

    Antimatter -> [ "Sugar bosons", "String theory", "Large macaron collider",
                    "Big bang bake", "Reverse cyclotrons", "Nanocosmics",
                    "The Pulse",
                    "Some other super-tiny fundamental particle? Probably?" ]

    Prism -> [ "Gem polish", "9th color", "Chocolate light", "Grainbow",
               "Pure cosmic light", "Glow-in-the-dark", "Lux sanctorum" ,
               "Reverse shadows" ]

noEffect :: Effect
noEffect _ st = st

eggBonus :: Effect
eggBonus _ = eggMultiplier +~ 0.01

prestigeBonus :: Double -> Effect
prestigeBonus n _ = prestigeMultiplier +~ n / 100

addEggTimeBonus :: Effect
addEggTimeBonus inp = eggMultiplier +~ views sessionLength eggTimeBonus inp

eggTimeBonus :: Double -> Double
eggTimeBonus s = (1 - (1 - day/100)**3) / 10
  where
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


saveFileToGameInput :: UTCTime -> SaveFile -> GameInput
saveFileToGameInput now sav = GameInput
  { _buildingsOwned     = bldgCurrent <$> savBuildings sav
  , _achievementsEarned = achievements
  , _upgradesBought     = upgradeList snd
  , _upgradesAvailable  = upgradeList inShop
  , _prestigeLevel      = savPrestige (savMain sav)
  , _sessionLength      = realToFrac (diffUTCTime now (savSessionStart (savStats sav)))
  , _cookiesMunched     = savMunched (savMain sav)
  , _cookiesBanked      = savCookies (savMain sav)
  , _dragonAura1        = dragonAuras !! savDragonAura (savMain sav)
  , _dragonAura2        = dragonAuras !! savDragonAura2 (savMain sav)
  }
  where
  inShop (unlocked,bought) = unlocked && not bought

  upgradeList f
     = fmap (upgradeById !!)
     $ findIndices f
     $ savUpgrades sav

  achievements
    = fmap (achievementById !!)
    $ findIndices id
    $ savAchievements sav
