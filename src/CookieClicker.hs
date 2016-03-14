{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language ForeignFunctionInterface #-}
{-# Language RankNTypes #-}

module CookieClicker where

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
import Control.Exception
import qualified Data.Text as Text

initialBuildingStat :: Double -> BuildingStat
initialBuildingStat base = BuildingStat
  { _bldgBase  = base
  , _bldgMult  = 1
  , _bldgBonus = 0
  , _bldgFree  = 0
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
  , _goldTimeMultiplier      = 1
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

cookieBonus :: Int -> Effect
cookieBonus pct = \_ -> multiplier *~ (1+fromIntegral pct/100)

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


data PayoffRow = PayoffRow
  { payoffName :: String
  , payoffMetric :: Double
  , payoffSaveup :: Double
  , payoffBuyAt :: Double
  , payoffDelta :: Double
  }

payoff :: GameInput -> GameState -> [PayoffRow]
payoff inp st =
     [ PayoffRow act (cost / delta) (cost / (7*900*cps)) (cost + reserve)
                        (delta / cps * 100)
     | (act, cost, f) <- buyBuilding ++ buyUpgrades ++ custom
     , let delta = effect f
     , delta > 0
     ]

  where
  reserve = 6000 * cps

  custom =
    [ finishA 500 Cursor
    , finishA 300 Shipment
    , finishA 300 AlchemyLab
    , finishA 300 Portal
    , finish True 250 TimeMachine "Cookietopian moments of maybe"
    , finish True 250 Antimatter "Some other super-tiny fundamental particle? Probably?" 
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
computeCps inp st = computeMultiplier inp st * (view bonusCps st + buildingCps)
  where
  buildingCps
    = sum
    $ Map.intersectionWith
        (\count cps -> fromIntegral count * cps)
        (view buildingsOwned inp)
        (computeBuildingCps st)

computeClickCookies :: GameInput -> GameState -> Double
computeClickCookies inp st = view mouseMultiplier st * cpc
  where
  cpc = computeCps inp st * view mouseBonus st
      + view (buildingBonus Cursor) st
      + view (buildingMult Cursor) st

loadMyInput :: IO GameInput
loadMyInput =
  do now <- getCurrentTime
     saveFileToGameInput now <$> loadMySave



countUpgrades :: GameInput -> Int
countUpgrades = length . filter (views upgradePool validPool) . view upgradesBought
  where
  validPool "" = True
  validPool "tech" = True
  validPool "cookie" = True
  validPool _ = False

computeMunched :: GameInput -> GameState -> Double
computeMunched input st = view wrinklerMultiplier st * view cookiesMunched input

computeWrinklerEffect :: GameInput -> GameState -> Double
computeWrinklerEffect input st = (1 - wither) + wither * view wrinklerMultiplier st * n
  where
  n = views wrinklers fromIntegral input
  wither = n * 0.05

data SuffixLength = LongSuffix | ShortSuffix

prettyNumber :: SuffixLength -> Double -> String
prettyNumber s n
  | n < 1e6   = numberWithSeparators (showFFloat (Just 1) n "")
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
  | otherwise = numberWithSeparators
              $ showFFloat (Just 3) (n / 1e48) (suffix " QiD" " quindecillion")
  where
  suffix short long =
    case s of
      ShortSuffix -> short
      LongSuffix  -> long

numberWithSeparators :: String -> String
numberWithSeparators str
  = case break ('.'==) str of
      (a,b) -> commas a ++ b
  where
  commas
    = reverse
    . intercalate ","
    . takeWhile (not . null)
    . map     (take 3)
    . iterate (drop 3)
    . reverse

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

gpoc :: Building -> Double -> Effect
gpoc b bonus = \inp ->
  let gmas = views (buildingOwned b) fromIntegral inp
  in buildingBase Grandma +~ bonus * gmas

upgradeEffects :: Map Text Effect
upgradeEffects = Map.fromList $
   [ (name, doubler b) | b <- [Grandma .. ], name <- buildingTieredUpgrades b ] ++
   [ (name, cookieBonus n) | (name, n) <- specialCookies ++ cookies ] ++
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

   , ("Lucky day"  , noEffect)
   , ("Serendipity", noEffect)
   , ("Get lucky"  , \_ -> goldTimeMultiplier *~ 2)

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
   , ("Omelette", noEffect)

   , ("A crumbly egg", noEffect)

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

   , ("Future almanacs"             , synergy Farm        TimeMachine)
   , ("Seismic magic"               , synergy Mine        WizardTower)
   , ("Quantum electronics"         , synergy Factory     Antimatter )
   , ("Contracts from beyond"       , synergy Bank        Portal     )
   , ("Paganism"                    , synergy Temple      Portal     )
   , ("Arcane knowledge"            , synergy WizardTower AlchemyLab )
   , ("Fossil fuels"                , synergy Mine        Shipment   )
   , ("Primordial ores"             , synergy Mine        AlchemyLab )
   , ("Infernal crops"              , synergy Farm        Portal     )
   , ("Extra physics funding"       , synergy Bank        Antimatter )
   , ("Relativistic parsec-skipping", synergy Shipment    TimeMachine)
   , ("Light magic"                 , synergy WizardTower Prism      )

   , ("Rain prayer"                 , synergy Farm        Temple     )
   , ("Asteroid mining"             , synergy Mine        Shipment   )
   , ("Temporal overclocking"       , synergy Factory     TimeMachine)
   , ("Printing presses"            , synergy Factory     Bank       )
   , ("God particle"                , synergy Temple      Antimatter )
   , ("Magical botany"              , synergy Farm        WizardTower)
   , ("Shipyards"                   , synergy Factory     Shipment   )
   , ("Gold fund"                   , synergy Bank        AlchemyLab )
   , ("Abysmal glimmer"             , synergy Portal      Prism      )
   , ("Primeval glow"               , synergy TimeMachine Prism      )
   , ("Chemical proficiency"        , synergy AlchemyLab  Antimatter )
   , ("Mystical energies"           , synergy Temple      Prism      )

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
   , ("Permanent upgrade slot III" , noEffect)
   , ("Permanent upgrade slot IIIV", noEffect)
   , ("Permanent upgrade slot V"   , noEffect)
   , ("Angels"                     , noEffect)
   , ("Archangels"                 , noEffect)
   , ("Virtues"                    , noEffect)
   , ("Dominions"                  , noEffect)
   , ("Cherubim"                   , noEffect)
   , ("Asmodeus"                   , noEffect)
   , ("Halo gloves"                , \_ -> mouseMultiplier *~ 1.1)
   , ("Unholy bait"                , noEffect)
   , ("Twin Gates of Transcendence", noEffect)
   , ("Heavenly luck"              , noEffect)
   , ("Lasting fortune"            , \_ -> goldTimeMultiplier *~ 1.1)

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

-- XXX: Implement "Starlove"
-- if (Game.Has('Starlove')) return 3; else return ",2;
specialCookies :: [(Text, Int)]
specialCookies =
   [ ("Pure heart biscuits",    2)
   , ("Ardent heart biscuits",  2)
   , ("Sour heart biscuits",    2)
   , ("Weeping heart biscuits", 2)
   , ("Golden heart biscuits",  2)
   , ("Eternal heart biscuits", 2)
   ]


cookies :: [(Text, Int)]
cookies =
   [ (view upgradeName u, n)
       | u <- upgradeById
       , "cookie" == view upgradePool u
       , Just n <- [view upgradePower u]
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

ceil' :: Double -> Double
ceil' = realToFrac . c_ceil . realToFrac

foreign import ccall "math.h floor" c_floor :: CDouble -> CDouble
foreign import ccall "math.h ceil" c_ceil :: CDouble -> CDouble

synergy :: Building -> Building -> Effect
synergy major minor inp
  = assert (major < minor)
  $ (buildingMult major *~ (1 + 0.05 * fromIntegral minorCount))
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
  , _wrinklers          = savWrinklers (savMain sav)
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

sellOff :: GameInput -> GameState -> Double
sellOff input st = view buildingCostMultiplier st * sums
  where
  cost1 n p = sum $ take n $ iterate (*1.15) p

  sums = sum $ Map.intersectionWith cost1 owned initialCosts

  owned = leftJoinWith' (-)
                (view buildingsOwned input)
                (view bldgFree <$> view buildingStats st)

computeElderFrenzyTime :: GameState -> Double
computeElderFrenzyTime st = ceil' (6 * view goldTimeMultiplier st)

-- | Cost factor to buy @count@ buildings when you have @start@ of them
-- already. Multiply this number by the initial cost of the building.
buildingRangeCostFactor :: Int -> Int -> Double
buildingRangeCostFactor start count = realToFrac res
  where
  res = 20 * z^start * ( z^count - 1 ) / 3 :: Rational
  z = 23/20

cpsToChainReserve6 :: Double -> Double
cpsToChainReserve6 cps = 4 * floor6 (m*cps)
  where
  m = 6 * 60 * 60

floor6 :: Double -> Double
floor6 x = last (6 : zs)
  where
  zs = takeWhile (<= x) $ map (2/3*) $ iterate (*10) 100
--var maxPayout=Math.min(Game.cookiesPs*60*60*6,Game.cookies*0.25)*mult;
