{-# LANGUAGE OverloadedStrings #-}
{-# Language ForeignFunctionInterface #-}
{-# Language RankNTypes #-}

module CookieClicker where

import GameInput
import Building
import SaveFormat
import SourceData
import Math

import Control.Monad (guard)
import Data.Text (Text)
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Lens hiding (ReifiedPrism(..), prism)
import Numeric.Lens
import Data.List
import Data.Ord
import Data.Time
import Foreign.C.Types (CDouble(..))
import Numeric
import Control.Exception
import qualified Data.Text as Text

initialBuildingStat :: Double -> BuildingStat
initialBuildingStat b = BuildingStat
  { _bldgBase  = b
  , _bldgMult  = 1
  , _bldgBonus = 0
  , _bldgFree  = 0
  }

initialGameState :: GameState
initialGameState = GameState
  { _buildingStats           = initialBuildingStat <$> baseCps
  , _multiplier              = 1
  , _mouseBonus              = 0
  , _mouseMultiplier         = 1
  , _prestigeMultiplier1     = 0
  , _prestigeMultiplier2     = 1
  , _bonusCps                = 0
  , _buildingCostMultiplier  = 1
  , _upgradeCostMultiplier   = 1
  , _milkMultiplier          = 1
  , _milkFactors             = []
  , _wrinklerMultiplier      = 1.1
  , _goldTimeMultiplier      = 1
  , _heartCookies            = 0
  , _heartCookieMultiplier   = 2
  , _cookieCostMultiplier    = 1
  , _synergyCostMultiplier   = 1

  , _goldenSwitchActive      = False
  , _goldenSwitchResidual    = False
  , _goldenSwitchBonus       = 0
  }

upgradeEffect :: Upgrade -> Effect
upgradeEffect = views upgradeName effectByName

effectByName :: Text -> Effect
effectByName n =
  Map.findWithDefault
    (trace ("Unknown effect: " ++ Text.unpack n) noEffect)
    n
    upgradeEffects

computeGameState :: GameInput -> GameState
computeGameState input
  = addPantheonEffects input
  $ effectByName (view dragonAura1 input) input
  $ effectByName (view dragonAura2 input) input
  $ foldl'
      (\acc u -> upgradeEffect u input acc)
      initialGameState
      (view upgradesBought input)

addPantheonEffects :: GameInput -> GameState -> GameState
addPantheonEffects inp =
  let PantheonSave slot1 slot2 slot3 = inp ^. pantheon
  in addPantheonEffect slot1 1 inp
   . addPantheonEffect slot2 2 inp
   . addPantheonEffect slot3 3 inp

addPantheonEffect :: Int {- ^ effect ID -} -> Int {- ^ level -} -> GameInput -> GameState -> GameState
addPantheonEffect (-1) = \_ _ st -> st
addPantheonEffect effectId = pantheonEffects !! effectId

type Effect = GameInput -> GameState -> GameState

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
    in (buildingMult building *~ (1 + bonus / 100))
     . (buildingMult Grandma  *~ 2)

doubler :: Building -> GameInput -> GameState -> GameState
doubler k _ = buildingMult k *~ 2

computeBuildingStatCps :: Int -> BuildingStat -> Double
computeBuildingStatCps lvl stat = stat^.bldgBonus + stat^.bldgMult * stat^.bldgBase * (1 + fromIntegral lvl/100)

computeBuildingCps :: GameInput -> GameState -> Map Building Double
computeBuildingCps inp st =
  Map.intersectionWith
    computeBuildingStatCps
    (view buildingLevels inp)
    (view buildingStats st)

leftJoinWith' :: Ord k => (a -> b -> a) -> Map k a -> Map k b -> Map k a
leftJoinWith' f = Map.mergeWithKey (\_ x y -> Just $! f x y) id (\_ -> Map.empty)

buildingCosts :: GameInput -> GameState -> Map Building Double
buildingCosts inp st
  = fmap (\x -> ceil' (x * view buildingCostMultiplier st))
  $ leftJoinWith'
      (\b n -> b * 1.15 ^ max 0 n)
      initialCosts
      owned'
  where
  -- When you've sold your free buildings they don't
  -- get cheaper, hence the 'max 0' above
  owned' = leftJoinWith' (-)
             (view buildingsOwned inp)
             (view bldgFree <$> view buildingStats st)


data PayoffRow = PayoffRow
  { payoffName :: String
  , payoffCost :: !Double
  , payoffDelta :: !Double
  , payoffInput :: !GameInput
  , payoffIcon :: (Int,Int)
  }

payoff :: GameInput -> GameState -> [PayoffRow]
payoff inp st =
     [ PayoffRow
         { payoffName  = act
         , payoffCost  = cost
         , payoffDelta = delta / cps
         , payoffInput = i'
         , payoffIcon = icon
         }
     | (act, cost, f, icon)
            <- buyBuilding ++ buyUpgrades
            ++ buyGrandmas ++ buyUpgradeRequirements
            ++ buyAchievements
            ++ nextEverythingBiscuit
     , let i' = f inp
     , let delta = computeCps i' (computeGameState i') - cps
     , delta > 0
     ]

  where
  nextEverythingBiscuit = take 1 possibilities
    where
      fewestBuildings = minimum (view buildingsOwned inp)
      possibilities =
        [ -- label -------
          ( show qty <> " each + " <> Text.unpack uName
          -- cost --------
          , computeBuildingGoalCost qty inp st
          + view upgradeCost upgrade
          -- effect ------
          , (buildingsOwned . mapped %~ max qty         )
          . (upgradesBought          %~ cons upgrade    )
          . (achievementsEarned      %~ cons achievement)
          -- icon --------
          , view upgradeIcon upgrade)

          | (qty, achievementName, uName) <- everythingBiscuits
          , qty > fewestBuildings
          , not (any (\u -> uName == view upgradeName u) (view upgradesBought inp))
          , let upgrade =
                  Map.findWithDefault
                    (error ("Unknown upgrade: " ++ Text.unpack uName))
                    uName
                    upgradeByName
          , let achievement =
                  Map.findWithDefault
                    (error ("Unknown achievement: " ++ Text.unpack achievementName))
                     achievementName
                     achievementByName
          ]

  buyBuilding =
    [( buildingName x
     , cost
     , buildingOwned x .~ new
     , buildingIcons x
     )
    | (x, cost) <- Map.toList costs
    , let new = view (buildingOwned x) inp + 1
    -- only offer this if we aren't also 1 buy from an achievement
    , Just new /= fmap fst (Map.lookup x nextAchievements)
    ]

  buyUpgrades =
     [ ( views upgradeName Text.unpack u
       , computeUpgradeCost inp st u
       , upgradesBought %~ cons u
       , view upgradeIcon u
       )
     | u <- view upgradesAvailable inp
     ]

  buyGrandmas =
     [ finish 15 b up
     | view (buildingOwned Grandma) inp >= 1
     , (b, up) <- synergyGrandmas
     , view (buildingOwned b) inp < 15
     ]

  achievements = Set.fromList (map (view achievementName) (view achievementsEarned inp))
  costs = buildingCosts inp st
  cps   = computeCps inp st


  buyUpgradeRequirements =
     [ finish count b up
     | (b, (count, up) : _) <- Map.toList $
        Map.intersectionWith nextUpgrade (view buildingsOwned inp) upgradeRequirements
     ]

  nextAchievements
    = Map.mapMaybe candidate
    $ Map.intersectionWith
        nextUpgrade
        (view buildingsOwned inp)
        buildingAchievements
    where
    candidate m
      = listToMaybe
      $ do (count, aName) <- m
           guard (aName `Set.notMember` achievements)
           let a = Map.findWithDefault
                     (error ("Unknown achievement: " ++ Text.unpack aName))
                     aName
                     achievementByName
           return (count, a)

  buyAchievements =
    [ finishA count b a | (b, (count, a)) <- Map.toList nextAchievements ]

  nextUpgrade now options =
     [ (target, up)
         | (target, up) <- options
         , target > now
         , notElemOf (upgradesBought . folded . upgradeName) up inp
         ]

  finish :: Int -> Building -> Text -> (String, Double, GameInput -> GameInput, (Int,Int))
  finish n b up =
    ("+" ++ show n' ++ " " ++ buildingName b ++ " + " ++ Text.unpack up, cost, f
    , view upgradeIcon u)
    where
    fa = case Map.lookup b nextAchievements of
      Just (count, a) | count <= n -> achievementsEarned %~ cons a
      _ -> id
    u = Map.findWithDefault (error ("Unknown upgrade: " ++ Text.unpack up))
               up
               upgradeByName
    n' = n - view (buildingOwned b) inp
    cost = view upgradeCost u + buyMore n' (costs ^?! ix b)
    f = (upgradesBought %~ cons u)
      . fa
      . (buildingOwned b .~ n)

  finishA n b a = ("+" ++ show n' ++ " " ++ buildingName b, cost, f, buildingIcons b)
    where
    n' = n - view (buildingOwned b) inp
    cost = buyMore n' (costs ^?! ix b)
    f = (achievementsEarned %~ cons a)
      . (buildingOwned b .~ n)

buyMore :: Int -> Double -> Double
buyMore count nextPrice
  | count < 0 = error "buyMore: negative count"
  | otherwise = ceil' (nextPrice * (1 - 1.15 ^ count) / (1 - 1.15))

-- | Compute the cost of buying buildings such that you have at least
-- the given number of each building.
computeBuildingGoalCost :: Int -> GameInput -> GameState -> Double
computeBuildingGoalCost n inp st =
    sum (Map.intersectionWith (\qty -> buyMore (max 0 (n - qty)))
                              (view buildingsOwned inp)
                              (buildingCosts inp st))


computeGoldenSwitchMultiplier :: GameState -> Double
computeGoldenSwitchMultiplier st
  | not active = 1
  | bonused    = 1.5 + bonus
  | otherwise  = 1.5
  where
  active  = view goldenSwitchActive st
  bonused = view goldenSwitchResidual st
  bonus   = view goldenSwitchBonus st

computeMultiplier :: GameInput -> GameState -> Double
computeMultiplier inp st
  = view multiplier st
  * computeGoldenSwitchMultiplier st
  * milkFactor
  * prestigeFactor
  * heartFactor

  where
  milkFactor = product [ 1 + milk * x | x <- view milkFactors st ]
  milk = computeMilk inp * view milkMultiplier st

  prestigeFactor = 1 + view prestigeMultiplier1 st
                     * view prestigeMultiplier2 st
                     * view prestigeLevel inp / 100
  heartFactor = (1 + view heartCookieMultiplier st / 100)
              ^ view heartCookies st

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
        (computeBuildingCps inp st)

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

data SuffixLength = LongSuffix | ShortSuffix

prettyNumber :: SuffixLength -> Double -> String
prettyNumber s n
  | isNaN n      = "NaN"
  | n < 0        = '-':prettyNumber s (-n)
  | isInfinite n = "Infinity"
  | n < 1e6   = numberWithSeparators (trimZero (showFFloat (Just 1) n ""))
  | otherwise =
        head [ showFFloat (Just 3) v (' ' : suffix)
             | (scale, suffix) <- zip [0..] names
             , let v = fromIntegral (round (n / 1000^scale)) / 1000 :: Double
             , v < 1000 ]
  where
  trimZero x | ['.','0'] `isSuffixOf` x = dropLast 2 x
             | otherwise = x
  dropLast i xs = zipWith const xs (drop i xs)

  names =
    case s of
      ShortSuffix -> shortNames
      LongSuffix  -> longNames

  shortPre  = ["","Un","Do","Tr","Qa","Qi","Sx","Sp","Oc","No"]
  shortPost = ["D","V","T","Qaa","Qia","Sxa","Spa","O","N","C"]
  shortNames = ["k","M","B","T","Qa","Qi","Sx","Sp","Oc","No"]
            ++ [ pre++post | post <- shortPost, pre <- shortPre ]

  longPre   = ["","un","duo","tre","quattuor","quin","sex","septen","octo","novem"]
  longPost  = ["decillion","vigintillion","trigintillion",
               "quadragintillion","quinquagintillion","sexagintillion",
               "septuagintillion","octogintillion","nonagintillion",
               "centillion"]
  longNames = ["thousand","million","billion","trillion","quadrillion",
               "quintillion","sextillion","septillion","octillion","nonillion"]
           ++ [ pre++post | post <- longPost, pre <- longPre ]

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

pantheonEffects :: [Int -> Effect]
pantheonEffects =
  -- asceticism
  [ \lvl -> let mult 1 = 15
                mult 2 = 10
                mult 3 =  5
            in cookieBonus (mult lvl)

  -- decadence
  , \lvl input ->
            let buildingEffect 1 = 0.93
                buildingEffect 2 = 0.95
                buildingEffect 3 = 0.98
            in buildingStats . mapped . bldgMult *~ buildingEffect lvl

  -- ruin
  , \_ -> noEffect -- Temporary click buff

  -- ages
  , \_ -> noEffect
     -- if (godLvl==1) mult*=1+0.15*Math.sin((Date.now()/1000/(60*60*3))*Math.PI*2);
     -- else if (godLvl==2) mult*=1+0.15*Math.sin((Date.now()/1000/(60*60*12))*Math.PI*2);
     -- else if (godLvl==3) mult*=1+0.15*Math.sin((Date.now()/1000/(60*60*24))*Math.PI*2);

  -- seasons (season switching more expensive)
  , \lvl _input ->
            let mult 1 = 1.3
                mult 2 = 1.2
                mult 3 = 1.1
            in heartCookieMultiplier *~ mult lvl

  -- creation
  , \lvl input ->
            let priceEffect 1 = 0.93
                priceEffect 2 = 0.95
                priceEffect 3 = 0.98
                heavenlyEffect 1 = 0.7
                heavenlyEffect 2 = 0.8
                heavenlyEffect 3 = 0.9
            in (buildingCostMultiplier *~ priceEffect lvl)
             . prestigeBonus2 (heavenlyEffect lvl) input

  -- labor
  , \lvl input ->
            let mouseEffect 1 = 1.15
                mouseEffect 2 = 1.10
                mouseEffect 3 = 1.05
                buildingEffect 1 = 0.97
                buildingEffect 2 = 0.98
                buildingEffect 3 = 0.99
            in (mouseMultiplier *~ mouseEffect lvl)
             . (buildingStats . mapped . bldgMult *~ buildingEffect lvl)

  -- industry (fewer cookies)
  , \lvl input ->
            let buildingEffect 1 = 1.10
                buildingEffect 2 = 1.05
                buildingEffect 3 = 1.03
            in buildingStats . mapped . bldgMult *~ buildingEffect lvl

  -- mother (fewer gold cookies)
  , \lvl _ -> let mult 1 = 1.1
                  mult 2 = 1.06
                  mult 3 = 1.03
              in milkMultiplier *~ mult lvl

  -- scorn (wrinklers appear faster)
  , \lvl _ -> let mult 1 = 1.15
                  mult 2 = 1.10
                  mult 3 = 1.05
              in wrinklerMultiplier *~ mult lvl

  -- order
  , \_ -> noEffect -- sugar lump rate
  ]

upgradeEffects :: Map Text Effect
upgradeEffects = Map.fromList $
   [ (name, doubler b) | b <- [Grandma .. ], name <- buildingTieredUpgrades b ] ++
   [ (name, cookieBonus n) | (name, n) <- cookies ] ++
   [ (name, \_ -> heartCookies +~ 1) | name <- heartCookieNames ] ++
   [ (name, cookieBonus 1) | name <- regularEasterEggs ] ++
   [ (name, grandmaType b n) | (n,(b,name)) <- zip [1..] synergyGrandmas ] ++
   [ (name, \_ -> mouseBonus +~ 0.01) | name <- mouseUpgrades ] ++
   [ (name, synergy minor major) | (name, minor, major) <- synergies ] ++

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

   -- KITTENS
   , ("Kitten helpers"    , kittenBonus 10)
   , ("Kitten workers"    , kittenBonus 12.5)
   , ("Kitten engineers"  , kittenBonus 15)
   , ("Kitten overseers"  , kittenBonus 17.5)
   , ("Kitten managers"   , kittenBonus 20)
   , ("Kitten accountants", kittenBonus 20)
   , ("Kitten specialists", kittenBonus 20)
   , ("Kitten experts"    , kittenBonus 20)
   , ("Kitten consultants", kittenBonus 20)
   , ("Kitten assistants to the regional manager", kittenBonus 17.5)
   , ("Kitten marketeers" , kittenBonus 15)
   , ("Kitten analysts"   , kittenBonus 12.5)
   , ("Kitten angels"     , kittenBonus 10)

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

   , ("Century egg"  , addEggTimeBonus)
   , ("Cookie egg"   , \_ -> mouseMultiplier *~ 1.1)
   , ("Wrinklerspawn", \_ -> wrinklerMultiplier *~ 1.05)

   , ("Faberge egg", \_ -> (buildingCostMultiplier *~ 0.99)
                         . (upgradeCostMultiplier  *~ 0.99) )
   , ("\"egg\"", \_ -> bonusCps +~ 9)
   , ("Omelette", noEffect)

   , ("A crumbly egg", noEffect)

   , ("Heavenly chip secret"  , prestigeBonus1 5)
   , ("Heavenly cookie stand" , prestigeBonus1 20)
   , ("Heavenly bakery"       , prestigeBonus1 25)
   , ("Heavenly confectionery", prestigeBonus1 25)
   , ("Heavenly key"          , prestigeBonus1 25)

   , ("Lucky digit"           , \st -> prestigeBonus2 1 st . residualBonus st)
   , ("Lucky number"          , \st -> prestigeBonus2 1 st . residualBonus st)
   , ("Lucky payout"          , \st -> prestigeBonus2 1 st . residualBonus st)

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
   , ("Santa's legacy"            , cookieBonus (15*3)) -- assumes max level 15
   , ("Santa's milk and cookies"  , \_ -> milkMultiplier *~ 1.05)
   , ("Santa's dominion"          , \inp -> cookieBonus 20 inp
                                         . (buildingCostMultiplier *~ 0.99)
                                         . (upgradeCostMultiplier *~ 0.98))

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
   , ("Permanent upgrade slot IV"  , noEffect)
   , ("Permanent upgrade slot V"   , noEffect)
   , ("Angels"                     , noEffect)
   , ("Archangels"                 , noEffect)
   , ("Virtues"                    , noEffect)
   , ("Dominions"                  , noEffect)
   , ("Cherubim"                   , noEffect)
   , ("Asmodeus"                   , noEffect)
   , ("Seraphim"                   , noEffect)
   , ("Beelzebub"                  , noEffect)
   , ("God"                        , noEffect)
   , ("Lucifer"                    , noEffect)
   , ("Chimera"                    , \_ -> synergyCostMultiplier *~ 0.98)
   , ("Fanciful dairy selection"   , noEffect)
   , ("Basic wallpaper assortment" , noEffect)
   , ("Lucifer"                    , noEffect)
   , ("Halo gloves"                , \_ -> mouseMultiplier *~ 1.1)
   , ("Unholy bait"                , noEffect)
   , ("Twin Gates of Transcendence", noEffect)

   , ("Get lucky"      , \inp -> (goldTimeMultiplier *~ 2) . residualBonus inp)
   , ("Lucky day"      , residualBonus)
   , ("Serendipity"    , residualBonus)
   , ("Heavenly luck"  , residualBonus)
   , ("Lasting fortune", \inp -> (goldTimeMultiplier *~ 1.1) . residualBonus inp)
   , ("Decisive fate"  , residualBonus)

   , ("Residual luck"              , \_ -> goldenSwitchResidual .~ True)

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
   , ("Golden switch [off]"    , \_ -> goldenSwitchActive .~ True)
   , ("Golden switch [on]"     , noEffect)
   , ("Milk selector"          , noEffect)
   , ("Golden goose egg"       , noEffect)
   , ("Chocolate egg"          , noEffect)

   -- Dragon Auras
   , ("No aura"         , noEffect)
   , ("Dragon Cursor"   , \_ -> mouseMultiplier *~ 1.05)
   , ("Elder Battalion" , elderBatallion)
   , ("Master of the Armory" , \_ -> upgradeCostMultiplier *~ 0.98)
   , ("Fierce Hoarder" , \_ -> buildingCostMultiplier *~ 0.98)
   , ("Breath of Milk", \_ -> milkMultiplier *~ 1.05 )
   , ("Dragon God", prestigeBonus2 5)
   , ("Radiant Appetite", cookieBonus 100)
   , ("Epoch Manipulator", \_ -> goldTimeMultiplier *~ 1.05)
   , ("Earth Shatterer",  noEffect)
   , ("Reaper of Fields",  noEffect)
   , ("Dragonflight"    , noEffect) -- effect not modeled
   , ("Mind Over Matter", noEffect) -- 0.75 multiplier to random drops

   , ("Divine discount", \_ -> buildingCostMultiplier *~ 0.99)
   , ("Divine sales", \_ -> upgradeCostMultiplier *~ 0.99)
   , ("Divine bakeries", \_ -> cookieCostMultiplier /~ 5)

   , ("Five-finger discount", fiveFingers)

   , ("Elder spice", noEffect)
   , ("Sacrilegious corruption", \_ -> wrinklerMultiplier *~ 1.05)

   , ("Starterror", noEffect)
   , ("Starspawn", noEffect)
   , ("Starsnow", noEffect)
   , ("Starlove", \_ -> heartCookieMultiplier *~ 1.5)
   , ("Startrade", noEffect)

   , ("Dragon's Fortune", noEffect)
   , ("Background selector", noEffect)
   , ("Golden cookie alert sound", noEffect)
   , ("Golden cookie sound selector", noEffect)
   , ("Heavenly cookies", cookieBonus 10)

   , ("Sugar baking", \inp -> cookieBonus (min 100 (inp ^. sugarLumps)) inp)
   , ("Sugar frenzy", noEffect)
   ]

elderBatallion :: Effect
elderBatallion inp =
  let count = sum (Map.delete Grandma (view buildingsOwned inp))
  in buildingMult Grandma +~ 1 + fromIntegral count/100

computeUpgradeCost :: GameInput -> GameState -> Upgrade -> Double
computeUpgradeCost inp st u = ceil' (poolMultiplier * c)
  where
  c = baseUpgradeCost inp st u
    * view upgradeCostMultiplier st

  synergyNames = view _1 <$> synergies

  poolMultiplier
    | view upgradePool u == "cookie" = view cookieCostMultiplier st
    | view upgradeName u `elem` synergyNames = view synergyCostMultiplier st
    | otherwise                      = 1

baseUpgradeCost :: GameInput -> GameState -> Upgrade -> Double
baseUpgradeCost inp st u =
  case view upgradeName u of
    "Golden switch [off]" -> 60 * 60 * computeCps inp st
    n | n `elem` santaUpgrades -> 2525 * 3 ^ view santaLevel inp
    _ -> view upgradeCost u

-- | Cookies with a constant power effect
cookies :: [(Text, Int)]
cookies =
   [ (view upgradeName u, n)
       | u <- upgradeById
       , "cookie" == view upgradePool u || "prestige" == view upgradePool u
       , Just n <- [view upgradePower u]
       ]

noEffect :: Effect
noEffect _ st = st

residualBonus :: Effect
residualBonus _ = goldenSwitchBonus +~ 0.1

prestigeBonus1 :: Double -> Effect
prestigeBonus1 n _ = prestigeMultiplier1 +~ n / 100

prestigeBonus2 :: Double -> Effect
prestigeBonus2 n _ = prestigeMultiplier2 *~ (1 + n / 100)

addEggTimeBonus :: Effect
addEggTimeBonus inp = multiplier *~ (1 + views sessionLength eggTimeBonus inp)

eggTimeBonus ::
  Double {- ^ current session duration in seconds -} ->
  Double {- ^ cookie production multiplier        -}
eggTimeBonus s = (1 - (1 - cappedDays/100)**3) / 10
  where
  secPerDay = 60 * 60 * 24

  -- this bonus increases in units of 10 seconds
  steppedSeconds = under (multiplying 10) floor' s

  days = steppedSeconds / secPerDay

  -- The benefit maxes out at 100 days
  cappedDays = min 100 days

synergy :: Building -> Building -> Effect
synergy major minor inp
  = assert (major < minor)
  $ (buildingMult major *~ (1 + 0.050 * fromIntegral minorCount))
  . (buildingMult minor *~ (1 + 0.001 * fromIntegral majorCount))
  where
  majorCount = view (buildingOwned major) inp
  minorCount = view (buildingOwned minor) inp

safeIndex :: String -> [a] -> Int -> a
safeIndex label xs i =
  case drop i xs of
    x:_ -> x
    []  -> error ("index error: " ++ label ++ "[" ++ show i ++ "]")

saveFileToGameInput :: UTCTime -> SaveFile -> GameInput
saveFileToGameInput now sav = GameInput
  { _buildingsOwned     = bldgCurrent <$> savBuildings sav
  , _buildingLevels     = bldgSpecial <$> savBuildings sav
  , _achievementsEarned = achievements
  , _upgradesBought     = upgradeList snd
  , _upgradesAvailable  = upgradeList inShop
  , _prestigeLevel      = savPrestige (savMain sav)
  , _sessionLength      = duration
  , _cookiesMunched     = savMunched (savMain sav) + savShinyMunched (savMain sav)
  , _wrinklers          = savWrinklers (savMain sav) -- shiny wrinklers are included
  , _cookiesBanked      = savCookies (savMain sav)
  , _dragonAura1        = safeIndex "dragonAuras" dragonAuras (savDragonAura (savMain sav))
  , _dragonAura2        = safeIndex "dragonAuras" dragonAuras (savDragonAura2 (savMain sav))
  , _santaLevel         = savSantaLevel (savMain sav)

  , _cookiesForfeit     = savCookiesReset  (savMain sav)
  , _cookiesEarned      = savCookiesEarned (savMain sav)
  , _heavenlyChips      = savHeavenlyChips (savMain sav)
  , _sugarLumps         = savSugarLumps (savMain sav)

  , _pantheon           = pantheonValue
  }
  where
  duration = realToFrac (diffUTCTime now (savSessionStart (savStats sav)))

  inShop (unlocked,bought) = unlocked && not bought

  upgradeList f
     = fmap (safeIndex "upgradeById" upgradeById)
     $ findIndices f
     $ savUpgrades sav

  achievements
    = fmap (safeIndex "achievementById" achievementById)
    $ findIndices id
    $ savAchievements sav

  pantheonValue =
    case Map.lookup Temple (savBuildings sav) of
      Just BuildingSave { bldgMinigame = str:_ } ->
        case parsePantheon str of
          Right x -> x
          Left e -> error e
      _ -> error "Pantheon missing"

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

-- | Compute cookies needed in bank to maximize the wrath-cookie chain
-- payout based on current cookies per second.
cpsToChainReserve6 ::
  Double {- ^ cookies per second -} ->
  Double {- ^ cookies to bank    -}
cpsToChainReserve6 cps = 4 * floor6 (6 * hours * cps)
  where
  hours = 60 * 60

floor6 :: Double -> Double
floor6 = under (powering 10 . multiplying (2/3)) (max 1 . floor')
--var maxPayout=Math.min(Game.cookiesPs*60*60*6,Game.cookies*0.25)*mult;

-- | Isomorphism between prestige level and cookies baked.
--
-- @
-- prestigeLevel = cookies**(1/3)/1e4
-- prestigeLevel = _Prestige # cookies
-- @
_Prestige :: Iso' Double Double
_Prestige = multiplying 1e4 . exponentiating 3


-- | Compute the cost to buy back the given number of buildings
-- after sacrificing that many for Krumblor, the cookie dragon.
sacrificeCost :: Int -> GameInput -> GameState -> Double
sacrificeCost n i st = sum (buyMore n <$> buildingCosts i' st)
  where
  i' = over (buildingsOwned . mapped) (subtract n) i

bigStep :: GameInput -> [String]
bigStep i
  | payoffCost best <= view cookiesBanked i
       = payoffName best
       : bigStep (payoffInput best & cookiesBanked -~ payoffCost best)
  | otherwise = []
  where
  best = minimumBy (comparing metric)
       $ payoff i (computeGameState i)

  metric x = payoffCost x / payoffDelta x + payoffCost x

computeWrinklerEffect :: GameInput -> GameState -> Double
computeWrinklerEffect input st =
  (1 - wither) + wither * view wrinklerMultiplier st * n
               / computeGoldenSwitchMultiplier st
  where
  n = views wrinklers fromIntegral input
  wither = n * 0.05

fiveFingers :: Effect
fiveFingers inp =
  upgradeCostMultiplier *~ 0.99**(fromIntegral cursors/ 100)
  where
  cursors = view (buildingOwned Cursor) inp

------------------------------------------------------------------------
-- Missing functions from lens
------------------------------------------------------------------------

-- | Divide a number identified by a setter by a divisor.
(/~) :: Fractional a => ASetter' s a -> a -> s -> s
l /~ x = over l (/ x)
{-# INLINE (/~) #-}

-- | The isomorphism between the power function and the log function
-- at a given base.
--
-- @
-- powering base = iso (base **) (logBase base)
-- @
powering :: Floating a => a -> Iso' a a
powering b = iso (b **) (logBase b)
