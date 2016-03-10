{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}

module GameInput where

import Building
import AesonTH

import Data.Text (Text)
import Data.Map (Map)
import Control.Lens

data GameInput = GameInput
  { _buildingsOwned     :: !(Map Building Int)
  , _achievementsEarned :: ![Achievement]
  , _upgradesBought     :: ![Upgrade]
  , _upgradesAvailable  :: ![Upgrade]
  , _prestigeLevel      :: !Double
  , _sessionLength      :: !Double
  , _cookiesMunched     :: !Double
  , _dragonAura1        :: !Text
  , _dragonAura2        :: !Text
  }
  deriving (Read, Show)

data BuildingStat = BuildingStat
  { _bldgBase, _bldgMult, _bldgBonus :: !Double
  , _bldgFree :: !Int
  }
  deriving (Show, Read)

data Upgrade = Upgrade
  { _upgradeName   :: !Text
  , _upgradeCost   :: !Double
  }
  deriving (Read, Show)

data Achievement = Achievement
  { _achievementName :: !Text
  , _achievementPool :: !Text
  }
  deriving (Read, Show)

data GameState = GameState
  { _buildingStats   :: !(Map Building BuildingStat)
  , _multiplier      :: !Double
  , _eggMultiplier   :: !Double
  , _prestigeMultiplier :: !Int
  , _mouseBonus      :: !Double
  , _mouseMultiplier :: !Double
  , _bonusCps        :: !Double
  , _buildingCostMultiplier  :: !Double
  , _upgradeCostMultiplier  :: !Double
  , _milkMultiplier         :: !Double
  , _milkFactors            :: ![Double]
  , _wrinklerMultiplier      :: !Double
  }
  deriving (Read, Show)

makeLenses ''Achievement
makeLenses ''BuildingStat
makeLenses ''GameInput
makeLenses ''GameState
makeLenses ''Upgrade

buildingStat :: Building -> Lens' GameState BuildingStat
buildingStat k = buildingStats . singular (ix k)
{-# INLINE buildingStat #-}

buildingOwned :: Building -> Lens' GameInput Int
buildingOwned k = buildingsOwned . at k . non 0
{-# INLINE buildingOwned #-}

buildingMult :: Building -> Lens' GameState Double
buildingMult k = buildingStat k . bldgMult
{-# INLINE buildingMult #-}

buildingBonus :: Building -> Lens' GameState Double
buildingBonus k = buildingStat k . bldgBonus
{-# INLINE buildingBonus #-}

buildingBase :: Building -> Lens' GameState Double
buildingBase k = buildingStat k . bldgBase
{-# INLINE buildingBase #-}

buildingFree :: Building -> Lens' GameState Int
buildingFree k = buildingStat k . bldgFree
{-# INLINE buildingFree #-}

myDeriveJSON ''Upgrade
  [ ("_upgradeName", "name")
  , ("_upgradeCost","price")
  ]

myDeriveJSON ''Achievement
  [ ("_achievementName", "name")
  , ("_achievementPool", "pool")
  ]
