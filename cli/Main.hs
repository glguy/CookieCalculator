{-# LANGUAGE RecordWildCards #-}
module Main where

import CookieClicker
import GameInput

import Control.Lens
import Spreadsheet
import Spreadsheet.Renderer
import Spreadsheet.Sorting
import System.FSNotify
import System.FilePath
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

payoffRowToSS PayoffRow{..} =
  [ StringV payoffName
  , NumberV (toRational (payoffCost / payoffDelta))
  , StringV (prettyNumber ShortSuffix payoffCost)
  , StringV (prettyNumber ShortSuffix payoffDelta)
  ]

payoffSS input st =
  renderSpreadsheet
  $ sortSpreadsheet
  $ Spreadsheet [ Column "Action" StringT Nothing
                , Column "Metric" (NumberT (Just 0)) (Just Descending)
                , Column "Cost" StringT Nothing
                , Column "∆% C/s" StringT Nothing
                ]
                (payoffRowToSS <$> payoff input st)
  where
  cps = computeCps input st

report :: GameInput -> IO ()
report input =
  do let st = computeGameState input
     putStrLn (payoffSS input st)
     let cps = computeCps input st
         ecps = cps * computeWrinklerEffect input st
         munched = computeMunched input st
     putStrLn $ "Buildings:   "  ++ show (sum (view buildingsOwned input))
     putStrLn $ "Bank/Munch:  "  ++ prettyNumber LongSuffix (view cookiesBanked input)
             ++ "  +  "          ++ prettyNumber LongSuffix munched
             ++ "  =  "          ++ prettyNumber LongSuffix (view cookiesBanked input + munched)
     putStrLn $ "Cookie/s:    "  ++ prettyNumber LongSuffix cps
             ++ "\t"             ++ prettyNumber LongSuffix (7*cps)
     putStrLn $ "Reserve:     "  ++ prettyNumber LongSuffix (6000*cps)
             ++ "\t"             ++ prettyNumber LongSuffix (7*6000*cps)
     putStrLn $ "Lucky:       "  ++ prettyNumber LongSuffix (900*cps)
             ++ "\t"             ++ prettyNumber LongSuffix (7*900*cps)
     putStrLn $ "ECookie/s:   "  ++ prettyNumber LongSuffix ecps
     putStrLn $ "ElderFenzy:  "  ++ prettyNumber LongSuffix
                                        (computeElderFrenzyTime st * ecps * 666)
     putStrLn $ "Cookie/c:    "  ++ prettyNumber LongSuffix (computeClickCookies input st)
     putStrLn $ "Upgrades:    "  ++ show (countUpgrades input)

main :: IO ()
main =
  withManager $ \mgr ->
  do let action = report =<< loadMyInput
         isSaveTxtEvent (Added    fp _) = takeFileName fp == "save.txt"
         isSaveTxtEvent (Modified fp _) = takeFileName fp == "save.txt"
         isSaveTxtEvent _               = False
     action
     watchDir mgr "." isSaveTxtEvent (\_ -> action)
     forever (threadDelay 1000000)

