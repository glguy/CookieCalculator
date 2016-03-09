module Building where

data Building
  = Cursor
  | Grandma
  | Farm
  | Mine
  | Factory
  | Bank
  | Temple
  | WizardTower
  | Shipment
  | AlchemyLab
  | Portal
  | TimeMachine
  | Antimatter
  | Prism
  deriving (Read, Show, Eq, Ord, Enum)
