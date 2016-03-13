{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module SaveFormat where

import           Building

import           Data.ByteString (ByteString)
import           Data.ByteString.Base64
import           Data.Char
import           Data.Map (Map)
import           Data.Text (Text)
import           Data.Text.Encoding
import           Data.Text.Read
import           Data.Time
import           Data.Time.Clock.POSIX
import           Numeric
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Text as Text

data BuildingSave = BuildingSave
  { bldgCurrent, bldgTotal, bldgSpecial :: Int
  , bldgBaked :: Double }
  deriving (Show)

-- NOTE: The order of the fields in SaveStats, SavePrefs, and SaveMain
-- must match the order found in the save format

data SaveStats = SaveStats
  { savSessionStart, savLegacyStart, savLastSave :: UTCTime
  , savName :: Text
  }
  deriving (Show)

data SavePrefs = SavePrefs
  { savParticles, savNumbers, savAutosave, savAutoupdate
  , savMilk, savFancy, savWarn, savCursors
  , savFocus, savFormat, savNotifs, savWobbly
  , savMonospace, savFilters, savCookieSound
  , savCrates :: Bool
  } deriving (Show)

data SaveMain = SaveMain
  { savCookies, savCookiesEarned :: Double
  , savCookieClicks, savGoldenClicks :: Int
  , savHandmadeCookies :: Double
  , savMissedGoldenClicks, savBackgroundType, savMilkType :: Int
  , savCookiesReset :: Double
  , savElderWrath, savPledges, savPledgesT, savNextResearch
  , savResearchT, savResets, savGoldenClicksLocal :: Int
  , savCookiesSucked :: Double
  , savWrinklersPopped, savSantaLevel, savReindeerClicked
  , savSeasonT, savSeasonUses :: Int
  , savSeason :: Text
  , savMunched :: Double
  , savWrinklers :: Int
  , savPrestige, savHeavenlyChips, savHeavenlyChipsSpent
  , savHeavenlyCookies :: Double
  , savAscensionMode :: Int
  , savPermUpgrade1, savPermUpgrade2
  , savPermUpgrade3, savPermUpgrade4, savPermUpgrade5
  , savDragonLevel, savDragonAura, savDragonAura2
  , savChimeType, savVolume :: Int
  } deriving (Show)


data SaveFile = SaveFile
  { savVersion :: Double
  , savReserved :: Text
  , savStats :: SaveStats
  , savPrefs :: SavePrefs
  , savMain :: SaveMain
  , savBuildings :: Map Building BuildingSave
  , savUpgrades :: [(Bool,Bool)] --(unlocked,bought)
  , savAchievements :: [Bool]
  }
  deriving (Show)

unescape :: String -> String
unescape ('%':x:y:z) =
  case readHex [x,y] of
    [(c,"")] -> chr c : unescape z
    _ -> error "unescape: bad escape"
unescape [] = []
unescape (x:xs) = x : unescape xs

removeEnd :: ByteString -> ByteString
removeEnd bs =
  case B.breakSubstring (B8.pack "!END!") bs of
    (a,b) | B.null b -> error "removeEnd: No end marker"
          | otherwise -> a

loadMySave :: IO SaveFile
loadMySave = loadSave =<< readFile "save.txt"

loadSave :: String -> IO SaveFile
loadSave raw =
  do let unesc = B8.pack (unescape raw)
         noend = removeEnd unesc
         utf8utf8 = Data.ByteString.Base64.decodeLenient noend
         txt = decodeUtf8 (B8.pack (Text.unpack (decodeUtf8 utf8utf8))) -- sorry, not my format
     either fail return $ parse txt

parseBldg :: Text -> Either String BuildingSave
parseBldg str =
  do let [bldgCurrentStr, bldgTotalStr, bldgBakedStr, bldgSpecialStr] = Text.splitOn "," str
     bldgCurrent <- fst <$> decimal bldgCurrentStr
     bldgTotal   <- fst <$> decimal bldgTotalStr
     bldgBaked   <- fst <$> rational bldgBakedStr
     bldgSpecial <- fst <$> decimal bldgSpecialStr
     return BuildingSave{..}

unpackBits :: Text -> [Bool]
unpackBits bs = Text.foldr (unpackOrd . ord) [] bs
  where
  unpackOrd n
    | n < 1     = error "Bad bit packing"
    | n == 1    = id
    | otherwise = unpackOrd (n`quot`2) . (odd n:)

toPairs :: [a] -> [(a,a)]
toPairs (x:y:z) = (x,y) : toPairs z
toPairs _       = []

integerToUTCTime :: Integer -> UTCTime
integerToUTCTime ms = posixSecondsToUTCTime (realToFrac s)
  where
  s = fromInteger ms / 1000 :: Rational

parsePrefs :: Text -> SavePrefs
parsePrefs x = SavePrefs{..}
  where
  [ savParticles, savNumbers, savAutosave, savAutoupdate
    , savMilk, savFancy, savWarn, savCursors
    , savFocus, savFormat, savNotifs, savWobbly
    , savMonospace, savFilters, savCookieSound
    , savCrates ] = unpackBits x

parse :: Text -> Either String SaveFile
parse str =
  do let [savVersionStr, savReserved,
            region1, region2, region3, region4, region5, region6]
            = Text.splitOn "|" str

     savVersion <- parser savVersionStr
     savStats <- populate (Text.splitOn ";" region1) SaveStats
     let savPrefs = parsePrefs region2

     savMain  <- populate (Text.splitOn ";" region3) SaveMain

     savBuildings <- Map.fromList . zip [Cursor ..]
              <$> traverse parseBldg (init (Text.splitOn ";" region4))

     let savUpgrades = toPairs $ unpackBits region5
         savAchievements = unpackBits region6

     return SaveFile{..}


class    HasParser a       where parser :: Text -> Either String a
instance HasParser Double  where parser x = fst <$> signed rational x
instance HasParser Int     where parser x = fst <$> signed decimal x
instance HasParser Text    where parser = Right
instance HasParser UTCTime where parser x = integerToUTCTime . fst <$> decimal x

class Populate a r where
  populate :: [Text] -> a -> Either String r

instance (HasParser a, Populate b r) => Populate (a -> b) r where
  populate [] _ = Left "Too few arguments"
  populate (x:xs) f = do g <- f <$> parser x
                         populate xs g

instance Populate r r where
  populate _ r = Right r
