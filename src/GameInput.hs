{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}

module GameInput where

import Building
import AesonTH

import Data.Text (Text)
import Data.Aeson (Value)
import qualified Data.Text as Text
import Data.Map (Map)
import Control.Lens
import Language.Haskell.TH.Syntax

data GameInput = GameInput
  { _buildingsOwned     :: !(Map Building Int)
  , _achievementsEarned :: ![Achievement]
  , _upgradesBought     :: ![Upgrade]
  , _upgradesAvailable  :: ![Upgrade]
  , _prestigeLevel      :: !Double
  , _sessionLength      :: !Double
  , _cookiesMunched     :: !Double
  , _wrinklers          :: !Int
  , _cookiesBanked      :: !Double
  , _dragonAura1        :: !Text
  , _dragonAura2        :: !Text
  , _santaLevel         :: !Int

  , _cookiesForfeit     :: !Double
  , _cookiesEarned      :: !Double
  , _heavenlyChips      :: !Double
  }
  deriving (Read, Show)

data BuildingStat = BuildingStat
  { _bldgBase, _bldgMult, _bldgBonus :: !Double
  , _bldgFree :: !Int
  }
  deriving (Show, Read)

data Upgrade = Upgrade
  { _upgradeName   :: !Text
  , _upgradePool   :: !Text
  , _upgradePower :: !(Maybe Int)
  , _upgradeCost  :: !Double
  , _upgradeIcon  :: {-# UNPACK #-}!(Int,Int)
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
  , _prestigeMultiplier :: !Double
  , _mouseBonus      :: !Double
  , _mouseMultiplier :: !Double
  , _bonusCps        :: !Double
  , _buildingCostMultiplier  :: !Double
  , _upgradeCostMultiplier  :: !Double
  , _milkMultiplier         :: !Double
  , _milkFactors            :: ![Double]
  , _wrinklerMultiplier      :: !Double
  , _goldTimeMultiplier :: !Double
  , _heartCookies :: !Int
  , _heartCookieMultiplier :: !Double
  , _cookieCostMultiplier :: !Double
  , _synergyCostMultiplier :: !Double

  , _goldenSwitchActive   :: !Bool
  , _goldenSwitchResidual :: !Bool
  , _goldenSwitchBonus    :: !Double
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
  , ("_upgradePool", "pool")
  , ("_upgradePower", "power")
  , ("_upgradeCost","price")
  , ("_upgradeIcon","icon")
  ]

myDeriveJSON ''Achievement
  [ ("_achievementName", "name")
  , ("_achievementPool", "pool")
  ]

instance Lift Text where
  lift txt = [| Text.pack str |]
    where
    str = Text.unpack txt

instance Lift Upgrade where
  lift (Upgrade v w x y z) = [| Upgrade v w x y z |]

instance Lift Achievement where
  lift (Achievement x y) = [| Achievement x y |]
