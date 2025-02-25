module Core.Database.VLatest where

import Prelude

import Core.Database.V1 as Prev
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

type UserState =
  { weapons :: Map WeaponName UserStateWeapon
  }

type UserStateWeapon =
  { ignored :: Boolean
  -- | The OB range owned by the user. E.g. if the user has this weapon at OB3, this range could be OB0-5.
  -- INVARIANT: this needs to match one of the items in the corresponding `WeaponData.distinctObs`.
  , ownedOb :: Maybe ObRange
  }

type SerializableUserState =
  { weapons :: MapAsArray WeaponName UserStateWeapon
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
newtype WeaponName = WeaponName NonEmptyString

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
  }

newtype ObRange = ObRange
  { from :: FromOb
  , to :: ToOb
  }

data FromOb = FromOb0 | FromOb1 | FromOb6 | FromOb10
data ToOb = ToOb0 | ToOb5 | ToOb9 | ToOb10

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
  , ranges :: Array GroupedWeaponRange
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

data EffectType
  = Heal { percentage :: Percentage }
  -- Buffs
  | PatkUp { durExt :: DurExt, potencies :: Potencies }
  | MatkUp { durExt :: DurExt, potencies :: Potencies }
  | PdefUp { durExt :: DurExt, potencies :: Potencies }
  | MdefUp { durExt :: DurExt, potencies :: Potencies }
  | FireDamageUp { durExt :: DurExt, potencies :: Potencies }
  | IceDamageUp { durExt :: DurExt, potencies :: Potencies }
  | ThunderDamageUp { durExt :: DurExt, potencies :: Potencies }
  | EarthDamageUp { durExt :: DurExt, potencies :: Potencies }
  | WaterDamageUp { durExt :: DurExt, potencies :: Potencies }
  | WindDamageUp { durExt :: DurExt, potencies :: Potencies }
  | Veil { durExt :: DurExt, percentage :: Percentage }
  | Provoke { durExt :: DurExt }
  -- Debuffs
  | PatkDown { durExt :: DurExt, potencies :: Potencies }
  | MatkDown { durExt :: DurExt, potencies :: Potencies }
  | PdefDown { durExt :: DurExt, potencies :: Potencies }
  | MdefDown { durExt :: DurExt, potencies :: Potencies }
  | FireResistDown { durExt :: DurExt, potencies :: Potencies }
  | IceResistDown { durExt :: DurExt, potencies :: Potencies }
  | ThunderResistDown { durExt :: DurExt, potencies :: Potencies }
  | EarthResistDown { durExt :: DurExt, potencies :: Potencies }
  | WaterResistDown { durExt :: DurExt, potencies :: Potencies }
  | WindResistDown { durExt :: DurExt, potencies :: Potencies }
  | Enfeeble { durExt :: DurExt }
  | Stop { durExt :: DurExt }
  | ExploitWeakness { durExt :: DurExt, percentage :: Percentage }

type WeaponEffect =
  { effectType :: EffectType
  , range :: Range
  }

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

derive instance Generic Range _
derive instance Generic EffectType _
derive instance Generic Potency _
derive instance Generic FilterEffectType _
derive instance Generic FromOb _
derive instance Generic ToOb _

derive instance Eq Range
derive instance Eq EffectType
derive instance Eq Potency
derive instance Eq FilterEffectType
derive instance Eq FromOb
derive instance Eq ToOb
derive newtype instance Eq Percentage
derive newtype instance Eq Duration
derive newtype instance Eq Extension
derive newtype instance Eq CharacterName
derive newtype instance Eq WeaponName
derive newtype instance Eq ObRange

derive instance Ord Potency
derive instance Ord EffectType
derive instance Ord Range
derive instance Ord FilterEffectType
derive instance Ord FromOb
derive instance Ord ToOb
derive newtype instance Ord Percentage
derive newtype instance Ord Duration
derive newtype instance Ord Extension
derive newtype instance Ord CharacterName
derive newtype instance Ord WeaponName
derive newtype instance Ord ObRange

derive instance Newtype Percentage _
derive instance Newtype Duration _
derive instance Newtype Extension _
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

instance Show FromOb where
  show = genericShow

instance Show ToOb where
  show = genericShow

derive newtype instance Show Percentage
derive newtype instance Show Duration
derive newtype instance Show Extension
derive newtype instance Show CharacterName
derive newtype instance Show WeaponName
derive newtype instance Show ObRange

instance WriteForeign Range where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign EffectType where
  writeImpl = J.genericWriteForeignTaggedSum TaggedSum.defaultOptions

instance WriteForeign Potency where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign FilterEffectType where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign FromOb where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign ToOb where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

derive newtype instance WriteForeign Percentage
derive newtype instance WriteForeign Duration
derive newtype instance WriteForeign Extension
derive newtype instance WriteForeign CharacterName
derive newtype instance WriteForeign WeaponName
derive newtype instance WriteForeign ObRange

instance ReadForeign Range where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign EffectType where
  readImpl = J.genericReadForeignTaggedSum TaggedSum.defaultOptions

instance ReadForeign Potency where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign FilterEffectType where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign FromOb where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign ToOb where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

derive newtype instance ReadForeign Percentage
derive newtype instance ReadForeign Duration
derive newtype instance ReadForeign Extension
derive newtype instance ReadForeign CharacterName
derive newtype instance ReadForeign WeaponName
derive newtype instance ReadForeign ObRange

instance Display WeaponName where
  display = display <<< unwrap

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

allPossiblePotencies :: Array Potency
allPossiblePotencies = Utils.listEnum

allFilterEffectTypes :: Array FilterEffectType
allFilterEffectTypes = Utils.listEnum

instance Display ObRange where
  display (ObRange { from, to }) = do
    let fromStr = displayFrom from
    let toStr = displayTo to
    if fromStr == toStr then "OB" <> displayFrom from
    else "OB" <> displayFrom from <> "-" <> displayTo to

    where
    displayFrom :: FromOb -> String
    displayFrom = case _ of
      FromOb0 -> "0"
      FromOb1 -> "1"
      FromOb6 -> "6"
      FromOb10 -> "10"

    displayTo :: ToOb -> String
    displayTo = case _ of
      ToOb0 -> "0"
      ToOb5 -> "5"
      ToOb9 -> "9"
      ToOb10 -> "10"

instance Display Potency where
  display =
    case _ of
      Low -> "Low"
      Mid -> "Mid"
      High -> "High"
      ExtraHigh -> "Extra High"
