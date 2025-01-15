module Core.Weapons.Types where

import Prelude

import Core.Display (class Display, display)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum
import Yoga.JSON.Generics.TaggedSumRep as TaggedSum

newtype CharacterName = CharacterName NonEmptyString
newtype WeaponName = WeaponName NonEmptyString

type Weapon =
  { name :: WeaponName
  , character :: CharacterName
  , source :: NonEmptyString
  , image :: NonEmptyString
  , ob0 :: ObLevel
  , ob1 :: ObLevel
  , ob6 :: ObLevel
  , ob10 :: ObLevel
  , cureAllAbility :: Boolean
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

derive instance Generic Range _
derive instance Generic EffectType _
derive instance Generic Potency _

derive instance Eq Range
derive instance Eq EffectType
derive instance Eq Potency
derive newtype instance Eq Percentage
derive newtype instance Eq CharacterName
derive newtype instance Eq WeaponName

derive instance Ord Potency
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

derive newtype instance Show Percentage
derive newtype instance Show CharacterName
derive newtype instance Show WeaponName

instance WriteForeign Range where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign EffectType where
  writeImpl = J.genericWriteForeignTaggedSum TaggedSum.defaultOptions

instance WriteForeign Potency where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

derive newtype instance WriteForeign Percentage
derive newtype instance WriteForeign CharacterName
derive newtype instance WriteForeign WeaponName

instance ReadForeign Range where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign EffectType where
  readImpl = J.genericReadForeignTaggedSum TaggedSum.defaultOptions

instance ReadForeign Potency where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

derive newtype instance ReadForeign Percentage
derive newtype instance ReadForeign CharacterName
derive newtype instance ReadForeign WeaponName

instance Display WeaponName where
  display = display <<< unwrap

instance Display CharacterName where
  display = display <<< unwrap
