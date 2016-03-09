{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SourceData where

import           AesonTH
import           GameInput

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Data.Map (Map)
import qualified Data.Map as Map


upgradeById :: [Upgrade]
upgradeById =
  $(do v <- loadAeson "upgrades.json"
       let parseUpgrade o = Upgrade <$> o .: "name" <*> o .: "price"
           upgradeE (Upgrade x y) = appsE [conE 'Upgrade, stringE (Text.unpack x), lift y]
       xs <- liftResult (parse (traverse parseUpgrade) v)
       listE (map upgradeE xs)
   )

upgradeByName :: Map Text Upgrade
upgradeByName = Map.fromList [ (view upgradeName u, u) | u <- upgradeById ]

achievementPoolById :: [Text]
achievementPoolById =
  $(do v  <- loadAeson "achievements.json"
       xs <- liftResult (parse (traverse (.: "pool")) v)
       lift (xs :: [String])
   )
