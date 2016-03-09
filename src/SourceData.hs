{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SourceData where

import           AesonTH

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

upgradeNameById :: [Text]
upgradeNameById =
  $(do v  <- loadAeson "upgrades.json"
       xs <- liftResult (parse (traverse (.: "name")) v)
       lift (xs :: [String])
   )

achievementPoolById :: [Text]
achievementPoolById =
  $(do v  <- loadAeson "achievements.json"
       xs <- liftResult (parse (traverse (.: "pool")) v)
       lift (xs :: [String])
   )
