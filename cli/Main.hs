module Main where

import CookieClicker

import System.FSNotify
import System.FilePath
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

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

