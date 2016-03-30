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

buildingName :: Building -> String
buildingName b =
  case b of
    Cursor -> "Cursor"
    Grandma -> "Grandma"
    Farm -> "Farm"
    Mine -> "Mine"
    Factory -> "Factory"
    Bank -> "Bank"
    Temple -> "Temple"
    WizardTower -> "Wizard tower"
    Shipment -> "Shipment"
    AlchemyLab -> "Alchemy lab"
    Portal -> "Portal"
    TimeMachine -> "Time machine"
    Antimatter -> "Antimatter condenser"
    Prism -> "Prism"
