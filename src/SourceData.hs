{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SourceData where

import           AesonTH
import           GameInput
import           Building
import           Math

import           Control.Lens hiding (Prism)
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

achievementByName :: Map Text Achievement
achievementByName = Map.fromList [ (view achievementName a, a) | a <- achievementById ]

dragonAuras :: [Text]
dragonAuras =
  [ "No aura", "Breath of Milk", "Dragon Cursor", "Elder Battalion",
    "Reaper of Fields", "Earth Shatterer", "Master of the Armory",
    "Fierce Hoarder", "Dragon God", "Arcane Aura", "Dragonflight",
    "Ancestral Metamorphosis", "Unholy Dominion", "Epoch Manipulator",
    "Mind Over Matter", "Radiant Appetite", "Dragon's Fortune"]

baseCps :: Map Building Double
baseCps = Map.fromList
  [ (Cursor,              0.1e0)
  , (Grandma,             1.0e0)
  , (Farm,                8.0e0)
  , (Mine,               47.0e0)
  , (Factory,           260.0e0)
  , (Bank,                1.4e3)
  , (Temple,              7.8e3)
  , (WizardTower,        44.0e3)
  , (Shipment,          260.0e3)
  , (AlchemyLab,          1.6e6)
  , (Portal,             10.0e6)
  , (TimeMachine,        65.0e6)
  , (Antimatter,        430.0e6)
  , (Prism,              2.90e9)
  , (Chancemaker,        21.0e9)
  , (FractalEngine,     150.0e9)
  , (JavascriptConsole,   1.1e12)
  ]

initialCosts :: Map Building Double
initialCosts = Map.fromList
  [ (Cursor,             15.0e0)
  , (Grandma,           100.0e0)
  , (Farm,                1.1e3)
  , (Mine,               12.0e3)
  , (Factory,           130.0e3)
  , (Bank,              1.400e6)
  , (Temple,             20.0e6)
  , (WizardTower,       330.0e6)
  , (Shipment,            5.1e9)
  , (AlchemyLab,         75.0e9)
  , (Portal,             1.0e12)
  , (TimeMachine,       14.0e12)
  , (Antimatter,       170.0e12)
  , (Prism,              2.1e15)
  , (Chancemaker,       26.0e15)
  , (FractalEngine,    310.0e15)
  , (JavascriptConsole,71.0e18)
  ]

baseCpsForTier :: Int -> Double
baseCpsForTier n = v1
  where
    n' = fromIntegral n
    v1 = ceil' (n' ** (n'*0.5+2) * 10) / 10

synergyGrandmas :: [(Building, Text)]
synergyGrandmas =
  [ (Farm       , "Farmer grandmas")
  , (Mine       , "Miner grandmas")
  , (Factory    , "Worker grandmas")
  , (Bank       , "Banker grandmas")
  , (Temple     , "Priestess grandmas")
  , (WizardTower, "Witch grandmas")
  , (Shipment   , "Cosmic grandmas")
  , (AlchemyLab , "Transmuted grandmas")
  , (Portal     , "Altered grandmas")
  , (TimeMachine, "Grandmas' grandmas")
  , (Antimatter , "Antigrandmas")
  , (Prism      , "Rainbow grandmas")
  , (Chancemaker, "Lucky grandmas")
  , (FractalEngine, "Metagrandmas")
  , (JavascriptConsole, "Binary grandmas")
  ]

santaUpgrades :: [Text]
santaUpgrades =
  ["Increased merriness", "Improved jolliness", "A lump of coal",
   "An itchy sweater", "Reindeer baking grounds", "Weighted sleighs",
   "Ho ho ho-flavored frosting", "Season savings", "Toy workshop",
   "Naughty list", "Santa's bottomless bag", "Santa's helpers",
   "Santa's legacy", "Santa's milk and cookies"]

mouseUpgrades :: [Text]
mouseUpgrades =
  ["Plastic mouse", "Iron mouse", "Titanium mouse", "Adamantium mouse",
   "Unobtainium mouse", "Eludium mouse", "Wishalloy mouse", "Fantasteel mouse",
   "Nevercrack mouse", "Armythril mouse", "Technobsidian mouse",
   "Plasmarble mouse"]


heartCookieNames :: [Text]
heartCookieNames =
   [ "Pure heart biscuits"
   , "Ardent heart biscuits"
   , "Sour heart biscuits"
   , "Weeping heart biscuits"
   , "Golden heart biscuits"
   , "Eternal heart biscuits"
   ]

regularEasterEggs :: [Text]
regularEasterEggs =
  ["Salmon roe" ,"Ant larva" ,"Cassowary egg", "Duck egg",
   "Turkey egg" ,"Turtle egg", "Quail egg", "Robin egg",
   "Ostrich egg", "Shark egg", "Chicken egg", "Frogspawn"]

buildingTieredUpgrades :: Building -> [Text]
buildingTieredUpgrades b =
  case b of
    Cursor ->
      []

    Grandma -> [
      "Forwards from grandma", "Steel-plated rolling pins",
      "Lubricated dentures", "Prune juice", "Double-thick glasses",
      "Aging agents", "Xtreme walkers", "The Unbridling",
      "Reverse dementia", "Timeproof hair dyes", "Good manners" ]

    Farm -> [
      "Cheap hoes", "Fertilizer", "Cookie trees",
      "Genetically-modified cookies", "Gingerbread scarecrows",
      "Pulsar sprinklers", "Fudge fungus", "Wheat triffids",
      "Humane pesticides", "Barnstars", "Lindworms"]

    Mine -> [
      "Sugar gas", "Megadrill", "Ultradrill", "Ultimadrill",
      "H-bomb mining", "Coreforge", "Planetsplitters",
      "Canola oil wells", "Mole people", "Mine canaries",
      "Bore again" ]

    Factory -> [
      "Sturdier conveyor belts", "Child labor", "Sweatshop",
      "Radium reactors", "Recombobulators", "Deep-bake process",
      "Cyborg workforce", "78-hour days", "Machine learning",
      "Brownie point system", "\"Volunteer\" interns" ]

    Bank -> [
      "Taller tellers", "Scissor-resistant credit cards",
      "Acid-proof vaults", "Chocolate coins",
      "Exponential interest rates", "Financial zen",
      "Way of the wallet", "The stuff rationale", "Edible money",
      "Grand supercycles", "Rules of acquisition" ]

    Temple -> [
      "Golden idols", "Sacrifices", "Delicious blessing",
      "Sun festival", "Enlarged pantheon", "Great Baker in the sky",
      "Creation myth", "Theocracy", "Sick rap prayers", "Psalm-reading",
      "War of the gods" ]

    WizardTower -> [
      "Pointier hats", "Beardlier beards", "Ancient grimoires",
      "Kitchen curses", "School of sorcery", "Dark formulas",
      "Cookiemancy", "Rabbit trick", "Deluxe tailored wands",
      "Immobile spellcasting", "Electricity" ]

    Shipment -> [
      "Vanilla nebulae", "Wormholes", "Frequent flyer",
      "Warp drive", "Chocolate monoliths", "Generation ship",
      "Dyson sphere", "The final frontier", "Autopilot",
      "Restaurants at the end of the universe", "Universal alphabet" ]

    AlchemyLab -> [
      "Antimony", "Essence of dough", "True chocolate",
      "Ambrosia", "Aqua crustulae", "Origin crucible",
      "Theory of atomic fluidity", "Beige goo", "The advent of chemistry",
      "On second thought", "Public betterment" ]

    Portal -> [
      "Ancient tablet", "Insane oatling workers", "Soul bond",
      "Sanity dance", "Brane transplant", "Deity-sized portals",
      "End of times back-up plan", "Maddening chants", "The real world",
      "Dimensional garbage gulper", "Embedded microportals" ]

    TimeMachine -> [
      "Flux capacitors", "Time paradox resolver",
      "Quantum conundrum", "Causality enforcer",
      "Yestermorrow comparators", "Far future enactment",
      "Great loop hypothesis", "Cookietopian moments of maybe",
      "Second seconds", "Additional clock hands", "Nostalgia" ]

    Antimatter -> [
      "Sugar bosons", "String theory", "Large macaron collider",
      "Big bang bake", "Reverse cyclotrons", "Nanocosmics",
      "The Pulse", "Some other super-tiny fundamental particle? Probably?",
      "Quantum comb", "Baking Nobel prize", "The definite molecule" ]

    Prism -> [
      "Gem polish", "9th color", "Chocolate light", "Grainbow",
      "Pure cosmic light", "Glow-in-the-dark", "Lux sanctorum" ,
      "Reverse shadows", "Crystal mirrors", "Reverse theory of light",
      "Light capture measures" ]

    Chancemaker -> [
      "Your lucky cookie", "\"All Bets Are Off\" magic coin",
      "Winning lottery ticket", "Four-leaf clover field",
      "A recipe book about books", "Leprechaun village",
      "Improbability drive", "Antisuperstistronics", "Bunnypedes",
      "Revised probabilistics", "0-sided dice" ]

    FractalEngine -> [
      "Metabakeries", "Mandelbrown sugar", "Fractoids",
      "Nested universe theory", "Menger sponge cake",
      "One particularly good-humored cow", "Chocolate ouroboros",
      "Nested", "Space-filling fibers", "Endless book of prose",
      "The set of all sets" ]

    JavascriptConsole -> [
      "The JavaScript console for dummies", "64bit arrays",
      "Stack overflow", "Enterprise compiler", "Syntactic sugar",
      "A nice cup of coffee", "Just-in-time baking", "cookies++",
      "Software updates", "Game.Loop", "eval()" ]


synergies :: [(Text, Building, Building)]
synergies =
   [ ("Future almanacs"             , Farm       , TimeMachine)
   , ("Seismic magic"               , Mine       , WizardTower)
   , ("Quantum electronics"         , Factory    , Antimatter )
   , ("Contracts from beyond"       , Bank       , Portal     )
   , ("Paganism"                    , Temple     , Portal     )
   , ("Arcane knowledge"            , WizardTower, AlchemyLab )
   , ("Fossil fuels"                , Mine       , Shipment   )
   , ("Primordial ores"             , Mine       , AlchemyLab )
   , ("Infernal crops"              , Farm       , Portal     )
   , ("Extra physics funding"       , Bank       , Antimatter )
   , ("Relativistic parsec-skipping", Shipment   , TimeMachine)
   , ("Light magic"                 , WizardTower, Prism      )
   , ("Charm quarks"                , Antimatter , Chancemaker)
   , ("Recursive mirrors"           , Prism      , FractalEngine)

   , ("Rain prayer"                 , Farm       , Temple     )
   , ("Asteroid mining"             , Mine       , Shipment   )
   , ("Temporal overclocking"       , Factory    , TimeMachine)
   , ("Printing presses"            , Factory    , Bank       )
   , ("God particle"                , Temple     , Antimatter )
   , ("Magical botany"              , Farm       , WizardTower)
   , ("Shipyards"                   , Factory    , Shipment   )
   , ("Gold fund"                   , Bank       , AlchemyLab )
   , ("Abysmal glimmer"             , Portal     , Prism      )
   , ("Primeval glow"               , TimeMachine, Prism      )
   , ("Chemical proficiency"        , AlchemyLab , Antimatter )
   , ("Mystical energies"           , Temple     , Prism      )
   , ("Gemmed talismans"            , Mine       , Chancemaker)
   , ("Mice clicking mice"          , Cursor     , FractalEngine)

   , ("Script grannies", Grandma, JavascriptConsole)
   , ("Tombola computing", Chancemaker, JavascriptConsole)
   ]

buildingIcons :: Building -> (Int,Int)
buildingIcons Cursor = (0,0)
buildingIcons Grandma = (1,0)
buildingIcons Farm = (2,0)
buildingIcons Mine = (3,0)
buildingIcons Factory = (4,0)
buildingIcons Shipment = (5,0)
buildingIcons AlchemyLab = (6,0)
buildingIcons Portal = (7,0)
buildingIcons TimeMachine = (8,0)
buildingIcons Antimatter = (13,0)
buildingIcons Prism = (14,0)
buildingIcons Bank = (15,0)
buildingIcons Temple = (16,0)
buildingIcons WizardTower = (17,0)
buildingIcons Chancemaker = (19,0)
buildingIcons FractalEngine = (20,0)
buildingIcons JavascriptConsole = (32,0)




buildingAchievements :: Map Building [(Int, Text)]
buildingAchievements = Map.fromList
   [ (Cursor,
      [ (1, "Click")
      , (2, "Double-click")
      , (50, "Mouse wheel")
      , (100, "Of Mice and Men")
      , (200, "The Digital")
      , (300, "Extreme polydactyly")
      , (400, "Dr. T")
      , (500, "Thumbs, phalanges, metacarpals")
      ])
   , (Grandma,
      [ (1, "Grandma's cookies")
      , (50, "Sloppy kisses")
      , (100, "Retirement home")
      , (150, "Friend of the ancients")
      , (200, "Ruler of the ancients")
      , (250, "The old never bothered me anyway")
      , (300, "The agemaster")
      , (350, "To oldly go")
      , (400, "Aged well")
      , (450, "101st birthday")
      , (500, "Defense of the ancients")
      , (550, "But wait 'til you get older")
      ])
   , (Farm,
      [ (1, "My first farm")
      , (50, "Reap what you sow")
      , (100, "Farm ill")
      , (150, "Perfected agriculture")
      , (200, "Homegrown")
      , (250, "Gardener extraordinaire")
      , (300, "Seedy business")
      , (350, "You and the beanstalk")
      , (400, "Harvest moon")
      , (450, "Make like a tree")
      , (500, "Sharpest tool in the shed")
      ])
   , (Mine,
      [ (1, "You know the drill")
      , (50, "Excavation site")
      , (100, "Hollow the planet")
      , (150, "Can you dig it")
      , (200, "The center of the Earth")
      , (250, "Tectonic ambassador")
      , (300, "Freak fracking")
      , (350, "Romancing the stone")
      , (400, "Mine?")
      , (450, "Cave story")
      , (500, "Hey now, you're a rock")
      ])
   , (Factory,
      [ (1, "Production chain")
      , (50, "Industrial revolution")
      , (100, "Global warming")
      , (150, "Ultimate automation")
      , (200, "Technocracy")
      , (250, "Rise of the machines")
      , (300, "Modern times")
      , (350, "Ex machina")
      , (400, "In full gear")
      , (450, "In-cog-neato")
      , (500, "Break the mold")
      ])
   , (Bank,
      [ (1, "Pretty penny")
      , (50, "Fit the bill")
      , (100, "A loan in the dark")
      , (150, "Need for greed")
      , (200, "It's the economy, stupid")
      , (250, "Acquire currency")
      , (300, "The nerve of war")
      , (350, "And I need it now")
      , (400, "Treacle tart economics")
      , (450, "Save your breath because that's all you've got left")
      , (500, "Get the show on, get paid")
      ])
   , (Temple,
      [ (1, "Your time to shrine")
      , (50, "Shady sect")
      , (100, "New-age cult")
      , (150, "Organized religion")
      , (200, "Fanaticism")
      , (250, "Zealotry")
      , (300, "Wololo")
      , (350, "Pray on the weak")
      , (400, "Holy cookies, grandma!")
      , (450, "Vengeful and almighty")
      , (500, "My world's on fire, how about yours")
      ])
   , (WizardTower,
      [ (1, "Bewitched")
      , (50, "The sorcerer's apprentice")
      , (100, "Charms and enchantments")
      , (150, "Curses and maledictions")
      , (200, "Magic kingdom")
      , (250, "The wizarding world")
      , (300, "And now for my next trick, I'll need a volunteer from the audience")
      , (350, "It's a kind of magic")
      , (400, "The Prestige")
      , (450, "Spell it out for you")
      , (500, "The meteor men beg to differ")
      ])
   , (Shipment,
      [ (1, "Expedition")
      , (50, "Galactic highway")
      , (100, "Far far away")
      , (150, "Type II civilization")
      , (200, "We come in peace")
      , (250, "Parsec-masher")
      , (300, "It's not delivery")
      , (350, "Make it so")
      , (400, "That's just peanuts to space")
      , (450, "Space space space space space")
      , (500, "Only shooting stars")
      ])
   , (AlchemyLab,
      [ (1, "Transmutation")
      , (50, "Transmogrification")
      , (100, "Gold member")
      , (150, "Gild wars")
      , (200, "The secrets of the universe")
      , (250, "The work of a lifetime")
      , (300, "Gold, Jerry! Gold!")
      , (350, "All that glitters is gold")
      , (400, "Worth its weight in lead")
      , (450, "Don't get used to yourself, you're gonna have to change")
      , (500, "We could all use a little change")
      ])
   , (Portal,
      [ (1, "A whole new world")
      , (50, "Now you're thinking")
      , (100, "Dimensional shift")
      , (150, "Brain-split")
      , (200, "Realm of the Mad God")
      , (250, "A place lost in time")
      , (300, "Forbidden zone")
      , (350, "H̸̷͓̳̳̯̟͕̟͍͍̣͡ḛ̢̦̰̺̮̝͖͖̘̪͉͘͡ ̠̦͕̤̪̝̥̰̠̫̖̣͙̬͘ͅC̨̦̺̩̲̥͉̭͚̜̻̝̣̼͙̮̯̪o̴̡͇̘͎̞̲͇̦̲͞͡m̸̩̺̝̣̹̱͚̬̥̫̳̼̞̘̯͘ͅẹ͇̺̜́̕͢s̶̙̟̱̥̮̯̰̦͓͇͖͖̝͘͘͞")
      , (400, "What happens in the vortex stays in the vortex")
      , (450, "Objects in the mirror dimension are closer than they appear")
      , (500, "Your brain gets smart but your head gets dumb")
      ])
   , (TimeMachine,
      [ (1, "Time warp")
      , (50, "Alternate timeline")
      , (100, "Rewriting history")
      , (150, "Time duke")
      , (200, "Forever and ever")
      , (250, "Heat death")
      , (300, "cookie clicker forever and forever a hundred years cookie clicker, all day long forever, forever a hundred times, over and over cookie clicker adventures dot com")
      , (350, "Way back then")
      , (400, "Invited to yesterday's party")
      , (450, "Groundhog day")
      , (500, "The years start coming")
      ])
   , (Antimatter,
      [ (1, "Antibatter")
      , (50, "Quirky quarks")
      , (100, "It does matter!")
      , (150, "Molecular maestro")
      , (200, "Walk the planck")
      , (250, "Microcosm")
      , (300, "Scientists baffled everywhere")
      , (350, "Exotic matter")
      , (400, "Downsizing")
      , (450, "A matter of perspective")
      , (500, "What a concept")
      ])
   , (Prism,
      [ (1, "Lone photon")
      , (50, "Dazzling glimmer")
      , (100, "Blinding flash")
      , (150, "Unending glow")
      , (200, "Rise and shine")
      , (250, "Bright future")
      , (300, "Harmony of the spheres")
      , (350, "At the end of the tunnel")
      , (400, "My eyes")
      , (450, "Optical illusion")
      , (500, "You'll never shine if you don't glow")
      ])
   , (Chancemaker,
      [ (1, "Lucked out")
      , (50, "What are the odds")
      , (100, "Grandma needs a new pair of shoes")
      , (150, "Million to one shot, doc")
      , (200, "As luck would have it")
      , (250, "Ever in your favor")
      , (300, "Be a lady")
      , (350, "Dicey business")
      , (400, "Maybe a chance in hell, actually")
      , (450, "Jackpot")
      , (500, "You'll never know if you don't go")
      ])
   , (FractalEngine,
      [ (1, "Self-contained")
      , (50, "Threw you for a loop")
      , (100, "The sum of its parts")
      , (150, "Bears repeating")
      , (200, "More of the same")
      , (250, "Last recurse")
      , (300, "Out of one, many")
      , (350, "An example of recursion")
      , (400, "For more information on this achievement, please refer to its title")
      , (450, "I\'m so meta, even this achievement")
      , (500, "Never get bored")
      ])
   , (JavascriptConsole,
      [ (1, "F12")
      , (50, "Variable success")
      , (100, "No comments")
      , (150, "Up to code")
      , (200, "Works on my machine")
      , (250, "Technical debt")
      , (300, "Mind your language")
      , (350, "Inconsolable")
      , (400, "Closure")
      , (450, "Dude what if we're all living in a simulation like what if we're all just code on a computer somewhere")
      , (500, "Taking the back streets")
      ])
   ]

upgradeRequirements :: Map.Map Building [(Int, Text)]
upgradeRequirements = Map.fromList
   [ (Cursor,
      [ (1, "Reinforced index finger")
      , (1, "Carpal tunnel prevention cream")
      , (10, "Ambidextrous")
      , (25, "Thousand fingers")
      , (50, "Million fingers")
      , (100, "Billion fingers")
      , (150, "Trillion fingers")
      , (200, "Quadrillion fingers")
      , (250, "Quintillion fingers")
      , (300, "Sextillion fingers")
      , (350, "Septillion fingers")
      , (400, "Octillion fingers")
      ])
   , (Grandma,
      [ (1, "Forwards from grandma")
      , (5, "Steel-plated rolling pins")
      , (25, "Lubricated dentures")
      , (50, "Prune juice")
      , (100, "Double-thick glasses")
      , (150, "Aging agents")
      , (200, "Xtreme walkers")
      , (250, "The Unbridling")
      , (300, "Reverse dementia")
      , (350, "Timeproof hair dyes")
      , (400, "Good manners")
      ])
   , (Farm,
      [ (1, "Cheap hoes")
      , (5, "Fertilizer")
      , (25, "Cookie trees")
      , (50, "Genetically-modified cookies")
      , (100, "Gingerbread scarecrows")
      , (150, "Pulsar sprinklers")
      , (200, "Fudge fungus")
      , (250, "Wheat triffids")
      , (300, "Humane pesticides")
      , (350, "Barnstars")
      , (400, "Lindworms")
      ])
   , (Mine,
      [ (1, "Sugar gas")
      , (5, "Megadrill")
      , (25, "Ultradrill")
      , (50, "Ultimadrill")
      , (100, "H-bomb mining")
      , (150, "Coreforge")
      , (200, "Planetsplitters")
      , (250, "Canola oil wells")
      , (300, "Mole people")
      , (350, "Mine canaries")
      , (400, "Bore again")
      ])
   , (Factory,
      [ (1, "Sturdier conveyor belts")
      , (5, "Child labor")
      , (25, "Sweatshop")
      , (50, "Radium reactors")
      , (100, "Recombobulators")
      , (150, "Deep-bake process")
      , (200, "Cyborg workforce")
      , (250, "78-hour days")
      , (300, "Machine learning")
      , (350, "Brownie point system")
      , (400, "\"Volunteer\" interns")
      ])
   , (Bank,
      [ (1, "Taller tellers")
      , (5, "Scissor-resistant credit cards")
      , (25, "Acid-proof vaults")
      , (50, "Chocolate coins")
      , (100, "Exponential interest rates")
      , (150, "Financial zen")
      , (200, "Way of the wallet")
      , (250, "The stuff rationale")
      , (300, "Edible money")
      , (350, "Grand supercycles")
      , (400, "Rules of acquisition")
      ])
   , (Temple,
      [ (1, "Golden idols")
      , (5, "Sacrifices")
      , (25, "Delicious blessing")
      , (50, "Sun festival")
      , (100, "Enlarged pantheon")
      , (150, "Great Baker in the sky")
      , (200, "Creation myth")
      , (250, "Theocracy")
      , (300, "Sick rap prayers")
      , (350, "Psalm-reading")
      , (400, "War of the gods")
      ])
   , (WizardTower,
      [ (1, "Pointier hats")
      , (5, "Beardlier beards")
      , (25, "Ancient grimoires")
      , (50, "Kitchen curses")
      , (100, "School of sorcery")
      , (150, "Dark formulas")
      , (200, "Cookiemancy")
      , (250, "Rabbit trick")
      , (300, "Deluxe tailored wands")
      , (350, "Immobile spellcasting")
      , (400, "Electricity")
      ])
   , (Shipment,
      [ (1, "Vanilla nebulae")
      , (5, "Wormholes")
      , (25, "Frequent flyer")
      , (50, "Warp drive")
      , (100, "Chocolate monoliths")
      , (150, "Generation ship")
      , (200, "Dyson sphere")
      , (250, "The final frontier")
      , (300, "Autopilot")
      , (350, "Restaurants at the end of the universe")
      , (400, "Universal alphabet")
      ])
   , (AlchemyLab,
      [ (1, "Antimony")
      , (5, "Essence of dough")
      , (25, "True chocolate")
      , (50, "Ambrosia")
      , (100, "Aqua crustulae")
      , (150, "Origin crucible")
      , (200, "Theory of atomic fluidity")
      , (250, "Beige goo")
      , (300, "The advent of chemistry")
      , (350, "On second thought")
      , (400, "Public betterment")
      ])
   , (Portal,
      [ (1, "Ancient tablet")
      , (5, "Insane oatling workers")
      , (25, "Soul bond")
      , (50, "Sanity dance")
      , (100, "Brane transplant")
      , (150, "Deity-sized portals")
      , (200, "End of times back-up plan")
      , (250, "Maddening chants")
      , (300, "The real world")
      , (350, "Dimensional garbage gulper")
      , (400, "Embedded microportals")
      ])
   , (TimeMachine,
      [ (1, "Flux capacitors")
      , (5, "Time paradox resolver")
      , (25, "Quantum conundrum")
      , (50, "Causality enforcer")
      , (100, "Yestermorrow comparators")
      , (150, "Far future enactment")
      , (200, "Great loop hypothesis")
      , (250, "Cookietopian moments of maybe")
      , (300, "Second seconds")
      , (350, "Additional clock hands")
      , (400, "Nostalgia")
      ])
   , (Antimatter,
      [ (1, "Sugar bosons")
      , (5, "String theory")
      , (25, "Large macaron collider")
      , (50, "Big bang bake")
      , (100, "Reverse cyclotrons")
      , (150, "Nanocosmics")
      , (200, "The Pulse")
      , (250, "Some other super-tiny fundamental particle? Probably?")
      , (300, "Quantum comb")
      , (350, "Baking Nobel prize")
      , (400, "The definite molecule")
      ])
   , (Prism,
      [ (1, "Gem polish")
      , (5, "9th color")
      , (25, "Chocolate light")
      , (50, "Grainbow")
      , (100, "Pure cosmic light")
      , (150, "Glow-in-the-dark")
      , (200, "Lux sanctorum")
      , (250, "Reverse shadows")
      , (300, "Crystal mirrors")
      , (350, "Reverse theory of light")
      , (400, "Light capture measures")
      ])
   , (Chancemaker,
      [ (1, "Your lucky cookie")
      , (5, "\"All Bets Are Off\" magic coin")
      , (25, "Winning lottery ticket")
      , (50, "Four-leaf clover field")
      , (100, "A recipe book about books")
      , (150, "Leprechaun village")
      , (200, "Improbability drive")
      , (250, "Antisuperstistronics")
      , (300, "Bunnypedes")
      , (350, "Revised probabilistics")
      , (400, "0-sided dice")
      ])
   , (FractalEngine,
      [ (1, "Metabakeries")
      , (5, "Mandelbrown sugar")
      , (25, "Fractoids")
      , (50, "Nested universe theory")
      , (100, "Menger sponge cake")
      , (150, "One particularly good-humored cow")
      , (200, "Chocolate ouroboros")
      , (250, "Nested")
      , (300, "Space-filling fibers")
      , (350, "Endless book of prose")
      , (400, "The set of all sets")
      ])
   , (JavascriptConsole,
      [ (1, "The JavaScript console for dummies")
      , (5, "64bit arrays")
      , (25, "Stack overflow")
      , (50, "Enterprise compiler")
      , (100, "Syntactic sugar")
      , (150, "A nice cup of coffee")
      , (200, "Just-in-time baking")
      , (250, "cookies++")
      , (300, "Software updates")
      , (350, "Game.Loop")
      , (400, "eval()")
      ])
   ]

-- | Achievements and cookies you unlock by having a certain number of every
-- building. List is (quantity, achievement, upgrade)
everythingBiscuits :: [(Int, Text, Text)]
everythingBiscuits =
  [ (100, "Centennial"                 , "Milk chocolate butter biscuit")
  , (150, "Centennial and a half"      , "Dark chocolate butter biscuit")
  , (200, "Bicentennial"               , "White chocolate butter biscuit")
  , (250, "Bicentennial and a half"    , "Ruby chocolate butter biscuit")
  , (300, "Tricentennial"              , "Lavender chocolate butter biscuit")
  , (350, "Tricentennial and a half"   , "Synthetic chocolate green honey butter biscuit")
  , (400, "Quadricentennial"           , "Royal raspberry chocolate butter biscuit")
  , (450, "Quadricentennial and a half", "Ultra-concentrated high-energy chocolate butter biscuit")
  , (500, "Quincentennial"             , "Pure pitch-black chocolate butter biscuit")
  ]
