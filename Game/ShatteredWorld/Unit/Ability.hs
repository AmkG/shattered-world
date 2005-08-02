
module Game.ShatteredWorld.Unit.Ability(
  Ability(..),
  abilityRandomize,
  abilityPrefixCandidates
  ) where

import Game.ShatteredWorld.Describe
import Game.ShatteredWorld.Unit.Class

import System.Random
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

data Ability =
    AbTough
  | AbSpeed
  | AbPower
  | AbFire
  | AbFireResist
  | AbFireResistAura
  | AbStun
  | AbImmobilize
  | AbTeleport
  | AbCloak
  | AbSight
  | AbSniper
  | AbPhysResist
  | AbMagicResist
  | AbReinforced
  | AbSourcery
  | AbCommandAura
  | AbShroud
  | AbRegeneration
  | AbRegenerationAura
  | AbExtendedRange
  | AbCrossbow
  | AbLightning
  | AbLightningResist
  deriving(Show, Eq, Ord, Read)

instance Describe Ability where
  describe AbTough            = "Increased life, but reduced movespeed and unit cap."
  describe AbSpeed            = "Increased movespeed, but reduced life and unit cap."
  describe AbPower            = "Increased damage, but reduced movespeed and unit cap."
  describe AbFire             = "Greatly increased damage and bonus against Structure, but is resisted by Fire Resist units and reduced unit cap."
  describe AbFireResist       = "Greatly reduced Fire damage, but increased cost."
  describe AbFireResistAura   = "Provides fire resistance to nearby units, but increased cost and vastly reduced unit cap."
  describe AbStun             = "Stuns enemy targets at each attack, but slight reduced damage and greatly reduced unit cap."
  describe AbImmobilize       = "Prevents enemy movement and teleportation at each attack, but reduced damage and unit cap."
  describe AbTeleport         = "Can teleport within a world fragment, but increased cost, reduced life and vastly reduced unit cap."
  describe AbCloak            = "Hidden from view unless discovered by Sight, but reduced life and reduced unit cap."
  describe AbSight            = "Has a chance of discovering cloaked units, but reduced unit cap."
  describe AbSniper           = "Greatly increased range and bonus to non-mechanical non-structure units, but greatly reduced unit cap, reduced fire rate, and increased cost."
  describe AbPhysResist       = "Reduced damage from Physical, but increased cost and slightly reduced unit cap."
  describe AbMagicResist      = "Reduced damage from Fire and Magic, but slightly reduced life."
  describe AbReinforced       = "Greatly increased life, but greatly increased cost."
  describe AbSourcery         = "Can provide mana supply to the world fragment naturally, but increased cost, reduced life, and vastly reduced unit cap."
  describe AbCommandAura      = "Increases damage of nearby units, but increases cost and vastly reduced unit cap."
  describe AbShroud           = "Slightly reduces ranged attack damage."
  describe AbRegeneration     = "Increased life recovery, but slightly increased cost and reduced unit cap."
  describe AbRegenerationAura = "Increased life recovery for nearby units, but increased cost and vastly reduced unit cap."
  describe AbExtendedRange    = "Increased attack range, but reduced unit cap and attack rate."
  describe AbCrossbow         = "Doubled unit cap and halved price, but reduced life and range, and greatly reduced attack rate."
  describe AbLightning        = "Deals damage to multiple units for each attack, but is resisted by Lightning Resist units, has reduced damage, and reduced unit cap."
  describe AbLightningResist  = "Stops lightning from propagating and slightly reduced Lightning damage, but increased cost."

{-
graphical effects:
AbTough: shield icon
AbSpeed: fast-forward icon
AbPower: chevron icon
AbFire: red-orange tint, also fire missiles (only on mage, tower, and archer)
AbFireResist: red-orange shroud
AbFireResistAura: red-orange foot sparklies
AbStun: black tint (only on knight)
AbImmobilize: rope icon
AbTeleport: blue twinkling
AbCloak: none (cloaked units are handled specially, and are 'see-through' while cloaked)
AbSight: eye icon
AbSniper: alternate sprite (only on archer)
AbPhysResist: black shroud
AbMagicResist: white shroud
AbReinforced: alternate sprite (only on tower)
AbSourcery: alternate sprite (only on mage)
AbCommandAura: alternate sprite (only on knight)
AbShroud: black twinkling
AbRegeneration: white twinkling
AbRegenerationAura: white foot sparklies
AbExtendedRange: none
AbCrossbow: alternate sprite (only on archer, not allowed to combine with AbSniper)
AbLightning: yellow tint (only on mage and tower, not allowed to combine with AbFire)
AbLightningResist: yellow shroud
-}

{-
When randomizing a new unit of a specific type, this provides a list of
candidate unit abilities.  Abilities earlier in the list have a better
chance of being chosen than later abilities.
-}
abilityList :: UnitClass -> [Ability]
abilityList UKnight = [ AbSpeed
                      , AbTough
                      , AbPower
                      , AbStun
                      , AbShroud
                      , AbRegeneration
                      , AbLightningResist
                      , AbCommandAura
                      , AbFireResistAura
                      ]
abilityList UArcher = [ AbSpeed
                      , AbFireResist
                      , AbImmobilize
                      , AbLightningResist
                      , AbCrossbow
                      , AbFire
                      , AbRegeneration
                      , AbCloak
                      , AbSniper
                      ]
abilityList UMage   = [ AbFire
                      , AbMagicResist
                      , AbFireResist
                      , AbSpeed
                      , AbTough
                      , AbLightning
                      , AbRegeneration
                      , AbTeleport
                      , AbSourcery
                      ]
abilityList UTower  = [ AbFire
                      , AbSight
                      , AbImmobilize
                      , AbLightning
                      , AbPower
                      , AbPhysResist
                      , AbExtendedRange
                      , AbFireResistAura
                      , AbRegenerationAura
                      ]

{-
Basic units of this type always have the given default ability.  Random
units have a 75% chance of having the default ability.
-}
abilityDefault :: UnitClass -> Ability
abilityDefault UKnight = AbPhysResist
abilityDefault UArcher = AbMagicResist
abilityDefault UMage   = AbPower
abilityDefault UTower  = AbReinforced

{-"Banned" combinations of abilities, either because:
1.  It would be too powerful, or
2.  The graphic effects would conflict.
-}
bannedCombos :: [(Ability, Ability)]
bannedCombos =
  [ (AbCloak, AbTeleport) -- too powerful
  , (AbFire, AbLightning) -- graphics conflict
  , (AbCrossbow, AbSniper) -- graphics conflict
  , (AbImmobilize, AbLightning) -- too powerful
  , (AbStun, AbLightning) -- too powerful
  , (AbCommandAura, AbFireResistAura) -- too powerful
  , (AbCommandAura, AbRegenerationAura) -- too powerful
  , (AbFireResistAura, AbRegenerationAura) -- too powerful
  , (AbSourcery, AbTeleport) -- too powerful
  ]

bannedMap :: Map.Map Ability (Set.Set Ability)
bannedMap = foldl' expandSet Map.empty (bannedCombos ++ map swap bannedCombos)
  where expandSet m (k, v) = Map.insertWith' Set.union k (Set.singleton v) m
        swap (a, b) = (b, a)

{-
randomization procedure:
75% chance of getting the default ability.
if default ability is taken, take 2 additional abilities.
otherwise, default ability will never appear and take 4 abilities.
-}
abilityRandomize :: RandomGen g => UnitClass -> g -> [Ability]
abilityRandomize uc = fst . runState make where
  make = do -- first make a random choice to get the default ability
            c1 <- rand; c2 <- rand
            if c1 && c2 then {-don't get the default ability-}
                             loop [] 4 [] (abilityList uc)
                        else {-do get the default ability-}
                             loop [abilityDefault uc] 2 [] (abilityList uc)
  -- chooses up to x abilities
  loop chosen 0 _       _         = return chosen
  loop chosen _ []      []        = return chosen -- out of abilities!
  loop chosen x skipped []        = {-try the skipped abilities again.  note that skipped was
                                      constructed reversed, so reverse it before trying again
                                    -}
                                    loop chosen x [] (reverse skipped)
  loop chosen x skipped (a:as)
    | any (isBanned a) chosen     = loop chosen x skipped as
    | otherwise                   = do c1 <- rand; c2 <- rand; c3 <- rand
                                       if c1 && c2 && c3 then {-get the ability-}
                                                              loop (a:chosen) (x-1) skipped as
                                                         else {-skip the ability-}
                                                              loop chosen x (a:skipped) as

  isBanned :: Ability -> Ability -> Bool
  isBanned a b = Set.member b (maybe Set.empty id (Map.lookup a bannedMap))

  rand = do g <- get
            let (rv, g2) = random g
            put g2
            return rv

{-
When a random unit is generated, its name may also refer to one of its abilities.
-}
abilityPrefixCandidates :: Ability -> [String]
abilityPrefixCandidates AbTough              = ["Entborn", "Oak", "Hearty"]
abilityPrefixCandidates AbSpeed              = ["Riverswift", "Ulfine", "Flying", "Fleeing"]
abilityPrefixCandidates AbPower              = ["Imperial", "Empyrian"]
abilityPrefixCandidates AbFire               = ["Fiery", "Impassioned", "Volcanic", "Ardent", "Blazing"]
abilityPrefixCandidates AbFireResist         = ["Phoenix", "Salamander", "Dragon", "Drake", "Wyvern"]
abilityPrefixCandidates AbFireResistAura     = ["Phoenix", "Salamander", "Dragon", "Drake", "Wyvern"]
abilityPrefixCandidates AbStun               = ["Ursic", "Beast", "Fiend", "Brute", "Despotic"]
abilityPrefixCandidates AbImmobilize         = ["Bramble", "Hailstone", "Brier", "Lasso"]
abilityPrefixCandidates AbTeleport           = ["Glyph", "Rune", "Cipher", "Enchanted"]
abilityPrefixCandidates AbCloak              = ["Night", "Wraith", "Shadow", "Silent", "Aphonic"]
abilityPrefixCandidates AbSight              = ["Seer", "Astute", "Perspicacious"]
abilityPrefixCandidates AbSniper             = ["Dread", "Doom", "Eagle", "Elite", "Prime", "Feared"]
abilityPrefixCandidates AbPhysResist         = ["Plated", "Iron"]
abilityPrefixCandidates AbMagicResist        = ["Serene", "Taciturn"]
abilityPrefixCandidates AbReinforced         = ["Bolstered", "Hardened"]
abilityPrefixCandidates AbSourcery           = ["Mystic", "Esoteric", "Inspired", "Worthy", "Exalted", "Energized", "Empowered"]
abilityPrefixCandidates AbCommandAura        = ["Commanding", "Eagle", "Kaiser", "Noble", "Superior", "Dominant", "Presidium", "Master"]
abilityPrefixCandidates AbShroud             = ["Cryptic", "Covered", "Elite", "Dignified", "Indomitable", "Unconquerable"]
abilityPrefixCandidates AbRegeneration       = []
abilityPrefixCandidates AbRegenerationAura   = ["Redemptive", "Restorative", "Healing", "Rejuvinative"]
abilityPrefixCandidates AbExtendedRange      = ["Tall", "Altitudinous", "Elevated", "Lofty"]
abilityPrefixCandidates AbCrossbow           = ["Numerous", "Peasant"]
abilityPrefixCandidates AbLightning          = ["Fulminating", "Electric", "Shocking"]
abilityPrefixCandidates AbLightningResist    = ["Grounded", "Eel"]
abilityPrefixCandidates _                    = []

