module Core.Weapons.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Yoga.JSON (class WriteForeign)
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum
import Yoga.JSON.Generics.TaggedSumRep as TaggedSum

type Weapon
  = { name :: NonEmptyString
    , character :: NonEmptyString
    , image :: NonEmptyString
    , ob0 :: ObLevel
    , ob1 :: ObLevel
    , ob6 :: ObLevel
    , ob10 :: ObLevel
    , cureAllAbility :: Boolean
    }

type ObLevel
  = { description :: NonEmptyString -- ^ The source text from which the buffs/debuffs were parsed.
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

newtype Percentage
  = Percentage Int

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

type WeaponEffect
  = { effectType ::
        EffectType
    , range ::
        Range
    }

type Potencies
  = { base :: Potency
    , max :: Potency
    }

derive instance genericRange :: Generic Range _

derive instance genericEffectType :: Generic EffectType _

derive instance genericPotency :: Generic Potency _

derive instance eqRange :: Eq Range

derive instance eqEffectType :: Eq EffectType

derive instance eqPotency :: Eq Potency

derive newtype instance eqPercentage :: Eq Percentage

instance showRange :: Show Range where
  show = genericShow

instance showEffectType :: Show EffectType where
  show = genericShow

instance showPotency :: Show Potency where
  show = genericShow

derive newtype instance showPercentage :: Show Percentage

instance writeForeignRange :: WriteForeign Range where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance writeForeignEffectType :: WriteForeign EffectType where
  writeImpl = J.genericWriteForeignTaggedSum TaggedSum.defaultOptions

instance writeForeignPotency :: WriteForeign Potency where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

derive newtype instance writeForeignPercentage :: WriteForeign Percentage
