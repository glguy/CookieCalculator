{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SourceData where

import           AesonTH
import           GameInput

import           Control.Lens
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Map as Map


upgradeById :: [Upgrade]
upgradeById =
  $(do xs <- loadAeson "upgrades.json"
       lift (xs :: [Upgrade])
   )

upgradeByName :: Map Text Upgrade
upgradeByName = Map.fromList [ (view upgradeName u, u) | u <- upgradeById ]

achievementById :: [Achievement]
achievementById =
  $(do xs <- loadAeson "achievements.json"
       lift (xs :: [Achievement])
   )

dragonAuras :: [Text]
dragonAuras =
  [ "No aura", "Breath of Milk", "Dragon Cursor", "Elder Battalion",
    "Reaper of Fields", "Earth Shatterer", "Master of the Armory",
    "Fierce Hoarder", "Dragon God", "Arcane Aura", "Dragonflight",
    "Ancestral Metamorphosis", "Unholy Dominion", "Epoch Manipulator",
    "Mind Over Matter", "Radiant Appetite"]
