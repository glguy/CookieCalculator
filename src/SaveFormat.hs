{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module SaveFormat where

import Building

import Data.Time
import Data.Time.Clock.POSIX
import Numeric
import Data.Char
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import Data.Bits
import Data.Text (Text)
import Data.Text.Read
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Map (Map)
import qualified Data.Map as Map

data BuildingSave = BuildingSave
  { bldgCurrent, bldgTotal, bldgSpecial :: Int
  , bldgBaked :: Double }
  deriving Show

data SaveFile = SaveFile
  { savVersion :: Text
  , savReserved :: Text
  , savSessionStart, savLegacyStart, savLastSave :: UTCTime
  , savName :: Text
  , savBank :: Text
  , savSessionBaked :: Double
  , savClicks :: Text
  , savGoldenLegacy :: Text
  , savHandMade :: Text
  , savMissedGolden :: Text
  , savUnknown2 :: Text
  , savUnknown3 :: Text
  , savForfeited :: Double
  , savWrath :: Text
  , savPledges :: Text
  , savUnknown4 :: Text
  , savUnknown5 :: Text
  , savUnknown6 :: Text
  , savSeasons :: Text
  , savGoldenSession :: Text
  , savMunchedTotal :: Double
  , savWrinklersPopped :: Int
  , savUnknown7 :: Text
  , savReindeer :: Text
  , savUnknown8 :: Text
  , savUnknown9 :: Text
  , savSeason :: Text
  , savMunched :: Double
  , savWrinklers :: Int
  , savUnknown10 :: Text
  , savUnknown11 :: Text
  , savUnknown12 :: Text
  , savUnknown13 :: Text
  , savUnknown14 :: Text
  , savUnknown15 :: Text
  , savUnknown16 :: Text
  , savUnknown17 :: Text
  , savUnknown18 :: Text
  , savUnknown19 :: Text
  , savUnknown20 :: Text
  , savUnknown21 :: Text
  , savUnknown22 :: Text
  , savUnknown23 :: Text
  , savUnknown24 :: Text
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

removeEnd bs =
  case B.breakSubstring (B8.pack "!END!") bs of
    (a,b) | B.null b -> error "removeEnd: No end marker"
          | otherwise -> a

loadMySave =
  do raw <- readFile "save.txt"
     let unesc = B8.pack (unescape raw)
         noend = removeEnd unesc
         Right utf8utf8 = Data.ByteString.Base64.decode noend
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
unpackBits bs = Text.foldr (aux . ord) [] bs

  where
  aux 0 _ = error "Bad bit packing"
  aux 1 rest = rest
  aux n rest | even n = aux (n`quot`2) (False : rest)
             | otherwise = aux (n`quot`2) (True : rest)

toPairs (x:y:z) = (x,y) : toPairs z
toPairs _       = []

integerToUTCTime :: Integer -> UTCTime
integerToUTCTime ms = posixSecondsToUTCTime (realToFrac s)
  where
  s = fromInteger ms / 1000 :: Rational

parse :: Text -> Either String SaveFile
parse str =
  do savBuildings <- Map.fromList . zip [Cursor ..] <$> traverse parseBldg (init (Text.splitOn ";" region4))
     savSessionStart <- integerToUTCTime . fst <$> decimal savSessionStartStr
     savLegacyStart  <- integerToUTCTime . fst <$> decimal savLegacyStartStr
     savLastSave     <- integerToUTCTime . fst <$> decimal savLastSaveStr

     savForfeited    <- fst <$> rational savForfeitedStr
     savSessionBaked <- fst <$> rational savSessionBakedStr

     savMunched      <- fst <$> rational savMunchedStr
     savMunchedTotal <- fst <$> rational savMunchedTotalStr
     savWrinklers    <- fst <$> decimal savWrinklersStr
     savWrinklersPopped <- fst <$> decimal savWrinklersPoppedStr
     return SaveFile{..}
  where
  [savVersion, savReserved, region1, region2, region3, region4, region5, region6] = Text.splitOn "|" str

  savUpgrades = toPairs $ unpackBits region5
  savAchievements = unpackBits region6

  [savSessionStartStr, savLegacyStartStr, savLastSaveStr, savName] = Text.splitOn ";" region1

  [savBank, savSessionBakedStr, savClicks, savGoldenLegacy,
   savHandMade, savMissedGolden, savUnknown2, savUnknown3, savForfeitedStr,
   savWrath, savPledges, savUnknown4, savUnknown5, savUnknown6, savSeasons,
   savGoldenSession, savMunchedTotalStr, savWrinklersPoppedStr, savUnknown7, savReindeer,
   savUnknown8, savUnknown9, savSeason, savMunchedStr, savWrinklersStr, savUnknown10,
   savUnknown11, savUnknown12, savUnknown13, savUnknown14, savUnknown15, savUnknown16,
   savUnknown17, savUnknown18, savUnknown19, savUnknown20, savUnknown21, savUnknown22,
   savUnknown23, savUnknown24] = init (Text.splitOn ";" region3)

