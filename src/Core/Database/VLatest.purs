module Core.Database.VLatest where

import Prelude

import Core.Display (class Display, display)
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

type Armory =
  { allWeapons :: Map WeaponName ArmoryWeapon
  , groupedByEffect :: Map Filter (Array GroupedWeapon)
  , allCharacterNames :: Set CharacterName
  }

type SerializableArmory =
  { allWeapons :: MapAsArray WeaponName ArmoryWeapon
  , groupedByEffect :: MapAsArray Filter (Array GroupedWeapon)
  , allCharacterNames :: SetAsArray CharacterName
  }

type GroupedWeapon =
  { weaponName :: WeaponName
  , potenciesAtOb10 :: Maybe Potencies
  }

newtype CharacterName = CharacterName NonEmptyString
newtype WeaponName = WeaponName NonEmptyString

type ArmoryWeapon =
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
  , ignored :: Boolean
  }

type ObLevel =
  { description :: NonEmptyString -- ^ The source text from which the buffs/debuffs were parsed.
  , effects ::
      Array WeaponEffect
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

data EffectType
  = Heal { percentage :: Percentage }
  -- Buffs
  | PatkUp Potencies
  | MatkUp Potencies
  | PdefUp Potencies
  | MdefUp Potencies
  | FireDamageUp Potencies
  | IceDamageUp Potencies
  | ThunderDamageUp Potencies
  | EarthDamageUp Potencies
  | WaterDamageUp Potencies
  | WindDamageUp Potencies
  | Veil
  | Provoke
  -- Debuffs
  | PatkDown Potencies
  | MatkDown Potencies
  | PdefDown Potencies
  | MdefDown Potencies
  | FireResistDown Potencies
  | IceResistDown Potencies
  | ThunderResistDown Potencies
  | EarthResistDown Potencies
  | WaterResistDown Potencies
  | WindResistDown Potencies
  | Enfeeble
  | Stop
  | ExploitWeakness

type WeaponEffect =
  { effectType ::
      EffectType
  , range ::
      Range
  }

type Potencies =
  { base :: Potency
  , max :: Potency
  }

type Filter =
  { effectType :: FilterEffectType
  , range :: FilterRange
  }

data FilterRange
  = FilterAll
  | FilterSingleTargetOrAll
  | FilterSelfOrSingleTargetOrAll

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

derive instance Generic Range _
derive instance Generic EffectType _
derive instance Generic Potency _
derive instance Generic FilterEffectType _
derive instance Generic FilterRange _

derive instance Eq Range
derive instance Eq EffectType
derive instance Eq Potency
derive instance Eq FilterEffectType
derive instance Eq FilterRange
derive newtype instance Eq Percentage
derive newtype instance Eq CharacterName
derive newtype instance Eq WeaponName

derive instance Ord Potency
derive instance Ord FilterEffectType
derive instance Ord FilterRange
derive newtype instance Ord CharacterName
derive newtype instance Ord WeaponName

derive instance Newtype Percentage _
derive instance Newtype CharacterName _
derive instance Newtype WeaponName _

instance Show Range where
  show = genericShow

instance Show EffectType where
  show = genericShow

instance Show Potency where
  show = genericShow

instance Show FilterEffectType where
  show = genericShow

instance Show FilterRange where
  show = genericShow

derive newtype instance Show Percentage
derive newtype instance Show CharacterName
derive newtype instance Show WeaponName

instance WriteForeign Range where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign FilterRange where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign EffectType where
  writeImpl = J.genericWriteForeignTaggedSum TaggedSum.defaultOptions

instance WriteForeign Potency where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign FilterEffectType where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

derive newtype instance WriteForeign Percentage
derive newtype instance WriteForeign CharacterName
derive newtype instance WriteForeign WeaponName

instance ReadForeign Range where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign FilterRange where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign EffectType where
  readImpl = J.genericReadForeignTaggedSum TaggedSum.defaultOptions

instance ReadForeign Potency where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign FilterEffectType where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

derive newtype instance ReadForeign Percentage
derive newtype instance ReadForeign CharacterName
derive newtype instance ReadForeign WeaponName

instance Display WeaponName where
  display = display <<< unwrap

instance Display CharacterName where
  display = display <<< unwrap

instance Display FilterRange where
  display = case _ of
    FilterAll -> "All"
    FilterSingleTargetOrAll -> "Single Target / All"
    FilterSelfOrSingleTargetOrAll -> "Self / Single Target / All"

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

allFilterRanges :: Array FilterRange
allFilterRanges = Utils.listEnum

allFilterEffectTypes :: Array FilterEffectType
allFilterEffectTypes = Utils.listEnum
