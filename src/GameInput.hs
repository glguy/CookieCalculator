{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}

module GameInput where

import Building

import Data.Text (Text)
import Data.Map (Map)
import Control.Lens

data GameInput = GameInput
  { _buildingsOwned     :: !(Map Building Int)
  , _achievementsEarned :: !Int
  , _upgradesBought     :: ![Upgrade]
  , _upgradesAvailable  :: ![Upgrade]
  , _prestigeLevel      :: !Int
  , _sessionLength      :: !Double
  , _cookiesMunched     :: !Double
  }

data BuildingStat = BuildingStat
  { _bldgBase, _bldgMult, _bldgBonus :: Double
  , _bldgFree :: Int
  }
  deriving (Show, Read, Eq)

data Upgrade = Upgrade
  { _upgradeName   :: Text
  , _upgradeCost   :: Double
  , _upgradeEffect :: GameInput -> GameState -> GameState
  }

data GameState = GameState
  { _buildingStats   :: !(Map Building BuildingStat)
  , _multiplier      :: !Double
  , _eggMultipliers  :: !Double
  , _prestigeMultiplier :: !Int
  , _mouseBonus      :: !Double
  , _mouseMultiplier :: !Double
  , _bonusCps        :: !Double
  , _buildingCostMultiplier  :: !Double
  , _upgradeCostMultiplier  :: !Double
  , _milkMultiplier         :: !Double
  , _milkFactors            :: ![Double]
  }
  deriving (Read, Show)

makeLenses ''GameState
makeLenses ''Upgrade
makeLenses ''GameInput
makeLenses ''BuildingStat

buildingStat :: Building -> Lens' GameState BuildingStat
buildingStat k = buildingStats . singular (ix k)

buildingOwned :: Building -> Lens' GameInput Int
buildingOwned k = buildingsOwned . at k . non 0

buildingMult :: Building -> Lens' GameState Double
buildingMult k = buildingStat k . bldgMult

buildingBonus :: Building -> Lens' GameState Double
buildingBonus k = buildingStat k . bldgBonus

buildingBase :: Building -> Lens' GameState Double
buildingBase k = buildingStat k . bldgBase

buildingFree :: Building -> Lens' GameState Int
buildingFree k = buildingStat k . bldgFree
