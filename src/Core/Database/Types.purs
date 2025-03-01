module Core.Database.Types where

import Prelude

import Core.Database.UserState.VLatest
import Core.Display (class Display, display)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Utils (MapAsArray, SetAsArray)
import Utils as Utils
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum
import Yoga.JSON.Generics.TaggedSumRep as TaggedSum

type DbState =
  { db :: Db
  , userState :: UserState
  }

type Db =
  { allWeapons :: Map WeaponName WeaponData
  , groupedByEffect :: Map FilterEffectType (Array GroupedWeapon)
  , allCharacterNames :: Set CharacterName
  }

type WeaponData =
  { weapon :: Weapon
  -- List of all groups of Overboost Levels with the same weapon effect potencies.
  --
  -- For most weapons, this will be [OB0-5, Ob6-Ob10]
  -- because they have the same potencies between Ob0 and Ob5,
  -- and between Ob6 and Ob10.
  , distinctObs :: NonEmptyArray ObRange
  }

type SerializableDb =
  { allWeapons :: MapAsArray WeaponName WeaponData
  , groupedByEffect :: MapAsArray FilterEffectType (Array GroupedWeapon)
  , allCharacterNames :: SetAsArray CharacterName
  }

newtype CharacterName = CharacterName NonEmptyString

type Weapon =
  { name :: WeaponName
  , character :: CharacterName
  , source :: NonEmptyString
  , image :: NonEmptyString
  , atbCost :: Int
  , ob0 :: ObLevel
  , ob1 :: ObLevel
  , ob6 :: ObLevel
  , ob10 :: ObLevel
  , cureAllAbility :: Boolean
  , commandAbilitySigil :: Maybe Sigil
  , sAbilities :: SAbilities
  , rAbilities :: RAbilities
  }

type SAbilities =
  { slot1 :: NonEmptyString
  , slot2 :: NonEmptyString
  , slot3 :: NonEmptyString
  }

type RAbilities =
  { slot1 :: NonEmptyString
  , slot2 :: NonEmptyString
  }

data Sigil
  = SigilO
  | SigilX
  | SigilTriangle
  | SigilDiamond

type ObLevel =
  { description :: NonEmptyString -- ^ The source text from which the buffs/debuffs were parsed.
  , effects :: Array WeaponEffect
  }

type GroupedWeapon =
  { weaponName :: WeaponName
  -- Most weapons have 1 range/potencies for each effect (i.e., this array will have exactly one item).
  --
  -- EXCEPTION: Yuffie's "Arctic Star" is an exception:
  -- it has PATK Up SingleTarget + PATK Up Mid->High Self
  --
  -- Silver Megaphone has PDEF Down Low SingleTarget + PDEF Down High SingleTarget (Condition: Critical Hit)
  --
  -- Some weapon effects have no ranges/potencies at all (e.g. Diamond Sigil),
  -- in which case this will be `Nothing`.
  --
  -- INVARIANT: at the moment, all effects with potencies also have a range.
  -- Therefore, we have potencies grouped by ranges.
  , ranges :: Maybe (Array GroupedWeaponRange)
  }

type GroupedWeaponRange =
  { range :: Range
  -- This will be `None` for effects that don't have potencies (e.g. `Heal`)
  , allPotencies :: Maybe AllPotencies
  }

type AllPotencies =
  { ob0 :: Potencies
  , ob1 :: Potencies
  , ob6 :: Potencies
  , ob10 :: Potencies
  }

data Potency
  = Low
  | Mid
  | High
  | ExtraHigh

data Range
  = All
  | SingleTarget
  | Self

newtype Percentage = Percentage Int
newtype Duration = Duration Int
newtype Extension = Extension Int

type DurExt =
  { duration :: Duration
  , extension :: Extension
  }

data WeaponEffect
  = Heal { range :: Range, percentage :: Percentage }
  -- Buffs
  | PatkUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | MatkUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | PdefUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | MdefUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | FireDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | IceDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | ThunderDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | EarthDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WaterDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WindDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | Veil { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | Provoke { range :: Range, durExt :: DurExt }
  -- Debuffs
  | PatkDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | MatkDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | PdefDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | MdefDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | FireResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | IceResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | ThunderResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | EarthResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WaterResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WindResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | Enfeeble { range :: Range, durExt :: DurExt }
  | Stop { range :: Range, durExt :: DurExt }
  | ExploitWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }

type Potencies =
  { base :: Potency
  , max :: Potency
  }

data FilterEffectType
  = FilterHeal

  | FilterVeil
  | FilterProvoke
  | FilterEnfeeble
  | FilterStop
  | FilterExploitWeakness

  | FilterPatkUp
  | FilterMatkUp
  | FilterPdefUp
  | FilterMdefUp
  | FilterFireDamageUp
  | FilterIceDamageUp
  | FilterThunderDamageUp
  | FilterEarthDamageUp
  | FilterWaterDamageUp
  | FilterWindDamageUp

  | FilterPatkDown
  | FilterMatkDown
  | FilterPdefDown
  | FilterMdefDown
  | FilterFireResistDown
  | FilterIceResistDown
  | FilterThunderResistDown
  | FilterEarthResistDown
  | FilterWaterResistDown
  | FilterWindResistDown

  | FilterSigilBoostO
  | FilterSigilBoostX
  | FilterSigilBoostTriangle
  | FilterSigilDiamond

derive instance Generic Range _
derive instance Generic WeaponEffect _
derive instance Generic Potency _
derive instance Generic FilterEffectType _
derive instance Generic Sigil _

derive instance Eq Range
derive instance Eq WeaponEffect
derive instance Eq Potency
derive instance Eq FilterEffectType
derive instance Eq Sigil
derive newtype instance Eq Percentage
derive newtype instance Eq Duration
derive newtype instance Eq Extension
derive newtype instance Eq CharacterName

derive instance Ord Potency
derive instance Ord WeaponEffect
derive instance Ord Range
derive instance Ord FilterEffectType
derive instance Ord Sigil
derive newtype instance Ord Percentage
derive newtype instance Ord Duration
derive newtype instance Ord Extension
derive newtype instance Ord CharacterName

derive instance Newtype Percentage _
derive instance Newtype Duration _
derive instance Newtype Extension _
derive instance Newtype CharacterName _

instance Show Range where
  show = genericShow

instance Show WeaponEffect where
  show = genericShow

instance Show Potency where
  show = genericShow

instance Show FilterEffectType where
  show = genericShow

instance Show Sigil where
  show = genericShow

derive newtype instance Show Percentage
derive newtype instance Show Duration
derive newtype instance Show Extension
derive newtype instance Show CharacterName

instance WriteForeign Range where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign WeaponEffect where
  writeImpl = J.genericWriteForeignTaggedSum TaggedSum.defaultOptions

instance WriteForeign Potency where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign FilterEffectType where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign Sigil where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

derive newtype instance WriteForeign Percentage
derive newtype instance WriteForeign Duration
derive newtype instance WriteForeign Extension
derive newtype instance WriteForeign CharacterName

instance ReadForeign Range where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign WeaponEffect where
  readImpl = J.genericReadForeignTaggedSum TaggedSum.defaultOptions

instance ReadForeign Potency where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign FilterEffectType where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign Sigil where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

derive newtype instance ReadForeign Percentage
derive newtype instance ReadForeign Duration
derive newtype instance ReadForeign Extension
derive newtype instance ReadForeign CharacterName

instance Display CharacterName where
  display = display <<< unwrap

instance Display FilterEffectType where
  display = case _ of
    FilterHeal -> "Heal"

    FilterVeil -> "Veil"
    FilterProvoke -> "Provoke"
    FilterEnfeeble -> "Enfeeble"
    FilterStop -> "Stop"
    FilterExploitWeakness -> "Exploit Weakness"

    FilterPatkUp -> "PATK up"
    FilterMatkUp -> "MATK up"
    FilterPdefUp -> "PDEF up"
    FilterMdefUp -> "MDEF up"
    FilterFireDamageUp -> "Fire damage up"
    FilterIceDamageUp -> "Ice damage up"
    FilterThunderDamageUp -> "Thunder damage up"
    FilterEarthDamageUp -> "Earth damage up"
    FilterWaterDamageUp -> "Water damage up"
    FilterWindDamageUp -> "Wind damage up"

    FilterPatkDown -> "PATK down"
    FilterMatkDown -> "MATK down"
    FilterPdefDown -> "PDEF down"
    FilterMdefDown -> "MDEF down"
    FilterFireResistDown -> "Fire resist down"
    FilterIceResistDown -> "Ice resist down"
    FilterThunderResistDown -> "Thunder resist down"
    FilterEarthResistDown -> "Earth resist down"
    FilterWaterResistDown -> "Water resist down"
    FilterWindResistDown -> "Wind resist down"

    FilterSigilBoostO -> "⏺ Sigil Boost"
    FilterSigilBoostX -> "✖ Sigil Boost"
    FilterSigilBoostTriangle -> "▲ Sigil Boost"
    FilterSigilDiamond -> "◆ Sigil"

allPossiblePotencies :: Array Potency
allPossiblePotencies = Utils.listEnum

allFilterEffectTypes :: Array FilterEffectType
allFilterEffectTypes = Utils.listEnum

instance Display Potency where
  display =
    case _ of
      Low -> "Low"
      Mid -> "Mid"
      High -> "High"
      ExtraHigh -> "Extra High"
