module Core.Database.Types where

import Core.Database.UserState.VLatest
import Prelude

import Core.Display (class Display, display)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Foreign (Foreign)
import Foreign as Foreign
import Utils (MapAsArray, SetAsArray, oneOf')
import Utils as Utils
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum

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
  , commandAbilitySigil :: Maybe Sigil
  , sAbilities :: SAbilities
  , rAbilities :: RAbilities
  , diamondCustomDescription :: Maybe NonEmptyString
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
  , heartCustomDescription :: Maybe NonEmptyString -- ^ The source text from which the buffs/debuffs were parsed.
  , spadeCustomDescription :: Maybe NonEmptyString -- ^ The source text from which the buffs/debuffs were parsed.
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
  -- The range an effect has at each overboost level.
  -- Most effects have the same range at all levels, but some don't:
  -- e.g. Festive Sword's Enliven is `Self` at OB0/OB1 and `All` at OB6/OB10.
  { allRanges :: AllRanges
  -- This will be `None` for effects that don't have potencies (e.g. `Heal`)
  , allPotencies :: Maybe AllPotencies
  }

type AllRanges =
  { ob0 :: Range
  , ob1 :: Range
  , ob6 :: Range
  , ob10 :: Range
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
  | HPGain { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | EnhanceBuffs { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | PhysicalWeaponBoost { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | MagicWeaponBoost { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | PhysicalDamageBonus { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | MagicDamageBonus { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | FireDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | IceDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | LightningDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | EarthDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WaterDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WindDamageUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | FireWeaponBoost { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | IceWeaponBoost { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | LightningWeaponBoost { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | EarthWeaponBoost { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | WaterWeaponBoost { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | WindWeaponBoost { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | FireDamageBonus { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | IceDamageBonus { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | LightningDamageBonus { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | EarthDamageBonus { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | WaterDamageBonus { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | WindDamageBonus { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | FireResistUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | IceResistUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | LightningResistUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | EarthResistUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WaterResistUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WindResistUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | Veil { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | Provoke { range :: Range, durExt :: DurExt }
  -- Debuffs
  | PatkDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | MatkDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | PdefDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | MdefDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | FireDamageDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | IceDamageDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | LightningDamageDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | EarthDamageDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WaterDamageDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WindDamageDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | FireResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | IceResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | LightningResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | EarthResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WaterResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | WindResistDown { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | FireWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | IceWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | LightningWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | EarthWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | WaterWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | WindWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | Enfeeble { range :: Range, durExt :: DurExt }
  | Stop { range :: Range, durExt :: DurExt }
  | ExploitWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | EnhanceDebuffs { range :: Range, durExt :: DurExt, potencies :: Potencies }
  | Enliven { range :: Range, durExt :: DurExt }

type Potencies =
  { base :: Potency
  , max :: Potency
  }

-- NOTE: the order of the variants of this type determines the order in which the filters are displayed in the UI.
data FilterEffectType
  = FilterHeal
  | FilterProvoke

  -- Buffs
  | FilterVeil
  | FilterPatkUp
  | FilterMatkUp
  | FilterPdefUp
  | FilterMdefUp
  | FilterHPGain
  | FilterEnhanceBuffs
  | FilterPhysicalWeaponBoost
  | FilterMagicWeaponBoost
  | FilterPhysicalDamageBonus
  | FilterMagicDamageBonus
  | FilterFireDamageUp
  | FilterIceDamageUp
  | FilterLightningDamageUp
  | FilterEarthDamageUp
  | FilterWaterDamageUp
  | FilterWindDamageUp
  | FilterFireResistUp
  | FilterIceResistUp
  | FilterLightningResistUp
  | FilterEarthResistUp
  | FilterWaterResistUp
  | FilterWindResistUp
  | FilterFireWeaponBoost
  | FilterIceWeaponBoost
  | FilterLightningWeaponBoost
  | FilterEarthWeaponBoost
  | FilterWaterWeaponBoost
  | FilterWindWeaponBoost
  | FilterFireDamageBonus
  | FilterIceDamageBonus
  | FilterLightningDamageBonus
  | FilterEarthDamageBonus
  | FilterWaterDamageBonus
  | FilterWindDamageBonus



  -- Debuffs
  | FilterEnfeeble
  | FilterStop
  | FilterExploitWeakness
  | FilterEnhanceDebuffs
  | FilterEnliven
  | FilterPatkDown
  | FilterMatkDown
  | FilterPdefDown
  | FilterMdefDown
  | FilterFireDamageDown
  | FilterIceDamageDown
  | FilterLightningDamageDown
  | FilterEarthDamageDown
  | FilterWaterDamageDown
  | FilterWindDamageDown
  | FilterFireResistDown
  | FilterIceResistDown
  | FilterLightningResistDown
  | FilterEarthResistDown
  | FilterWaterResistDown
  | FilterWindResistDown
  | FilterFireWeakness
  | FilterIceWeakness
  | FilterLightningWeakness
  | FilterEarthWeakness
  | FilterWaterWeakness
  | FilterWindWeakness

  | FilterSigilBoostO
  | FilterSigilBoostX
  | FilterSigilBoostTriangle
  | FilterSigilDiamond

derive instance Generic Range _
derive instance Generic Potency _
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
  show =
    case _ of
      Heal rec -> showRec rec "Heal"
      PatkUp rec -> showRec rec "PatkUp"
      MatkUp rec -> showRec rec "MatkUp"
      PdefUp rec -> showRec rec "PdefUp"
      MdefUp rec -> showRec rec "MdefUp"
      PhysicalWeaponBoost rec -> showRec rec "PhysicalWeaponBoost"
      MagicWeaponBoost rec -> showRec rec "MagicWeaponBoost"
      PhysicalDamageBonus rec -> showRec rec "PhysicalDamageBonus"
      MagicDamageBonus rec -> showRec rec "MagicDamageBonus"
      FireDamageUp rec -> showRec rec "FireDamageUp"
      IceDamageUp rec -> showRec rec "IceDamageUp"
      LightningDamageUp rec -> showRec rec "LightningDamageUp"
      EarthDamageUp rec -> showRec rec "EarthDamageUp"
      WaterDamageUp rec -> showRec rec "WaterDamageUp"
      WindDamageUp rec -> showRec rec "WindDamageUp"
      FireWeaponBoost rec -> showRec rec "FireWeaponBoost"
      IceWeaponBoost rec -> showRec rec "IceWeaponBoost"
      LightningWeaponBoost rec -> showRec rec "LightningWeaponBoost"
      EarthWeaponBoost rec -> showRec rec "EarthWeaponBoost"
      WaterWeaponBoost rec -> showRec rec "WaterWeaponBoost"
      WindWeaponBoost rec -> showRec rec "WindWeaponBoost"
      FireDamageBonus rec -> showRec rec "FireDamageBonus"
      IceDamageBonus rec -> showRec rec "IceDamageBonus"
      LightningDamageBonus rec -> showRec rec "LightningDamageBonus"
      EarthDamageBonus rec -> showRec rec "EarthDamageBonus"
      WaterDamageBonus rec -> showRec rec "WaterDamageBonus"
      WindDamageBonus rec -> showRec rec "WindDamageBonus"
      FireResistUp rec -> showRec rec "FireResistUp"
      IceResistUp rec -> showRec rec "IceResistUp"
      LightningResistUp rec -> showRec rec "LightningResistUp"
      EarthResistUp rec -> showRec rec "EarthResistUp"
      WaterResistUp rec -> showRec rec "WaterResistUp"
      WindResistUp rec -> showRec rec "WindResistUp"
      Veil rec -> showRec rec "Veil"
      Provoke rec -> showRec rec "Provoke"
      PatkDown rec -> showRec rec "PatkDown"
      MatkDown rec -> showRec rec "MatkDown"
      PdefDown rec -> showRec rec "PdefDown"
      MdefDown rec -> showRec rec "MdefDown"
      FireDamageDown rec -> showRec rec "FireDamageDown"
      IceDamageDown rec -> showRec rec "IceDamageDown"
      LightningDamageDown rec -> showRec rec "LightningDamageDown"
      EarthDamageDown rec -> showRec rec "EarthDamageDown"
      WaterDamageDown rec -> showRec rec "WaterDamageDown"
      WindDamageDown rec -> showRec rec "WindDamageDown"
      FireResistDown rec -> showRec rec "FireResistDown"
      IceResistDown rec -> showRec rec "IceResistDown"
      LightningResistDown rec -> showRec rec "LightningResistDown"
      EarthResistDown rec -> showRec rec "EarthResistDown"
      WaterResistDown rec -> showRec rec "WaterResistDown"
      WindResistDown rec -> showRec rec "WindResistDown"
      FireWeakness rec -> showRec rec "FireWeakness"
      IceWeakness rec -> showRec rec "IceWeakness"
      LightningWeakness rec -> showRec rec "LightningWeakness"
      EarthWeakness rec -> showRec rec "EarthWeakness"
      WaterWeakness rec -> showRec rec "WaterWeakness"
      WindWeakness rec -> showRec rec "WindWeakness"
      Enfeeble rec -> showRec rec "Enfeeble"
      Stop rec -> showRec rec "Stop"
      ExploitWeakness rec -> showRec rec "ExploitWeakness"
      HPGain rec -> showRec rec "HPGain"
      EnhanceBuffs rec -> showRec rec "EnhanceBuffs"
      EnhanceDebuffs rec -> showRec rec "EnhanceDebuffs"
      Enliven rec -> showRec rec "Enliven"
    where
    showRec :: forall rec. Show rec => rec -> String -> String
    showRec rec recType = recType <> " " <> show rec

instance Show Potency where
  show = genericShow

instance Show FilterEffectType where
  show = case _ of
    FilterHeal -> "FilterHeal"
    FilterVeil -> "FilterVeil"
    FilterProvoke -> "FilterProvoke"
    FilterEnfeeble -> "FilterEnfeeble"
    FilterStop -> "FilterStop"
    FilterExploitWeakness -> "FilterExploitWeakness"
    FilterHPGain -> "FilterHPGain"
    FilterEnhanceBuffs -> "FilterEnhanceBuffs"
    FilterEnhanceDebuffs -> "FilterEnhanceDebuffs"
    FilterEnliven -> "FilterEnliven"
    FilterPatkUp -> "FilterPatkUp"
    FilterMatkUp -> "FilterMatkUp"
    FilterPdefUp -> "FilterPdefUp"
    FilterMdefUp -> "FilterMdefUp"
    FilterPhysicalWeaponBoost -> "FilterPhysicalWeaponBoost"
    FilterMagicWeaponBoost -> "FilterMagicWeaponBoost"
    FilterPhysicalDamageBonus -> "FilterPhysicalDamageBonus"
    FilterMagicDamageBonus -> "FilterMagicDamageBonus"
    FilterFireDamageUp -> "FilterFireDamageUp"
    FilterIceDamageUp -> "FilterIceDamageUp"
    FilterLightningDamageUp -> "FilterLightningDamageUp"
    FilterEarthDamageUp -> "FilterEarthDamageUp"
    FilterWaterDamageUp -> "FilterWaterDamageUp"
    FilterWindDamageUp -> "FilterWindDamageUp"
    FilterFireResistUp -> "FilterFireResistUp"
    FilterIceResistUp -> "FilterIceResistUp"
    FilterLightningResistUp -> "FilterLightningResistUp"
    FilterEarthResistUp -> "FilterEarthResistUp"
    FilterWaterResistUp -> "FilterWaterResistUp"
    FilterWindResistUp -> "FilterWindResistUp"
    FilterFireWeaponBoost -> "FilterFireWeaponBoost"
    FilterIceWeaponBoost -> "FilterIceWeaponBoost"
    FilterLightningWeaponBoost -> "FilterLightningWeaponBoost"
    FilterEarthWeaponBoost -> "FilterEarthWeaponBoost"
    FilterWaterWeaponBoost -> "FilterWaterWeaponBoost"
    FilterWindWeaponBoost -> "FilterWindWeaponBoost"
    FilterFireDamageBonus -> "FilterFireDamageBonus"
    FilterIceDamageBonus -> "FilterIceDamageBonus"
    FilterLightningDamageBonus -> "FilterLightningDamageBonus"
    FilterEarthDamageBonus -> "FilterEarthDamageBonus"
    FilterWaterDamageBonus -> "FilterWaterDamageBonus"
    FilterWindDamageBonus -> "FilterWindDamageBonus"
    FilterPatkDown -> "FilterPatkDown"
    FilterMatkDown -> "FilterMatkDown"
    FilterPdefDown -> "FilterPdefDown"
    FilterMdefDown -> "FilterMdefDown"
    FilterFireDamageDown -> "FilterFireDamageDown"
    FilterIceDamageDown -> "FilterIceDamageDown"
    FilterLightningDamageDown -> "FilterLightningDamageDown"
    FilterEarthDamageDown -> "FilterEarthDamageDown"
    FilterWaterDamageDown -> "FilterWaterDamageDown"
    FilterWindDamageDown -> "FilterWindDamageDown"
    FilterFireResistDown -> "FilterFireResistDown"
    FilterIceResistDown -> "FilterIceResistDown"
    FilterLightningResistDown -> "FilterLightningResistDown"
    FilterEarthResistDown -> "FilterEarthResistDown"
    FilterWaterResistDown -> "FilterWaterResistDown"
    FilterWindResistDown -> "FilterWindResistDown"
    FilterFireWeakness -> "FilterFireWeakness"
    FilterIceWeakness -> "FilterIceWeakness"
    FilterLightningWeakness -> "FilterLightningWeakness"
    FilterEarthWeakness -> "FilterEarthWeakness"
    FilterWaterWeakness -> "FilterWaterWeakness"
    FilterWindWeakness -> "FilterWindWeakness"
    FilterSigilBoostO -> "FilterSigilBoostO"
    FilterSigilBoostX -> "FilterSigilBoostX"
    FilterSigilBoostTriangle -> "FilterSigilBoostTriangle"
    FilterSigilDiamond -> "FilterSigilDiamond"

instance Show Sigil where
  show = genericShow

derive newtype instance Show Percentage
derive newtype instance Show Duration
derive newtype instance Show Extension
derive newtype instance Show CharacterName

instance WriteForeign Range where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign WeaponEffect where
  writeImpl =
    case _ of
      Heal rec -> writeRecord rec "Heal"
      PatkUp rec -> writeRecord rec "PatkUp"
      MatkUp rec -> writeRecord rec "MatkUp"
      PdefUp rec -> writeRecord rec "PdefUp"
      MdefUp rec -> writeRecord rec "MdefUp"
      PhysicalWeaponBoost rec -> writeRecord rec "PhysicalWeaponBoost"
      MagicWeaponBoost rec -> writeRecord rec "MagicWeaponBoost"
      PhysicalDamageBonus rec -> writeRecord rec "PhysicalDamageBonus"
      MagicDamageBonus rec -> writeRecord rec "MagicDamageBonus"
      FireDamageUp rec -> writeRecord rec "FireDamageUp"
      IceDamageUp rec -> writeRecord rec "IceDamageUp"
      LightningDamageUp rec -> writeRecord rec "LightningDamageUp"
      EarthDamageUp rec -> writeRecord rec "EarthDamageUp"
      WaterDamageUp rec -> writeRecord rec "WaterDamageUp"
      WindDamageUp rec -> writeRecord rec "WindDamageUp"
      FireResistUp rec -> writeRecord rec "FireResistUp"
      IceResistUp rec -> writeRecord rec "IceResistUp"
      LightningResistUp rec -> writeRecord rec "LightningResistUp"
      EarthResistUp rec -> writeRecord rec "EarthResistUp"
      WaterResistUp rec -> writeRecord rec "WaterResistUp"
      WindResistUp rec -> writeRecord rec "WindResistUp"
      FireWeaponBoost rec -> writeRecord rec "FireWeaponBoost"
      IceWeaponBoost rec -> writeRecord rec "IceWeaponBoost"
      LightningWeaponBoost rec -> writeRecord rec "LightningWeaponBoost"
      EarthWeaponBoost rec -> writeRecord rec "EarthWeaponBoost"
      WaterWeaponBoost rec -> writeRecord rec "WaterWeaponBoost"
      WindWeaponBoost rec -> writeRecord rec "WindWeaponBoost"
      FireDamageBonus rec -> writeRecord rec "FireDamageBonus"
      IceDamageBonus rec -> writeRecord rec "IceDamageBonus"
      LightningDamageBonus rec -> writeRecord rec "LightningDamageBonus"
      EarthDamageBonus rec -> writeRecord rec "EarthDamageBonus"
      WaterDamageBonus rec -> writeRecord rec "WaterDamageBonus"
      WindDamageBonus rec -> writeRecord rec "WindDamageBonus"
      Veil rec -> writeRecord rec "Veil"
      Provoke rec -> writeRecord rec "Provoke"
      PatkDown rec -> writeRecord rec "PatkDown"
      MatkDown rec -> writeRecord rec "MatkDown"
      PdefDown rec -> writeRecord rec "PdefDown"
      MdefDown rec -> writeRecord rec "MdefDown"
      FireDamageDown rec -> writeRecord rec "FireDamageDown"
      IceDamageDown rec -> writeRecord rec "IceDamageDown"
      LightningDamageDown rec -> writeRecord rec "LightningDamageDown"
      EarthDamageDown rec -> writeRecord rec "EarthDamageDown"
      WaterDamageDown rec -> writeRecord rec "WaterDamageDown"
      WindDamageDown rec -> writeRecord rec "WindDamageDown"
      FireResistDown rec -> writeRecord rec "FireResistDown"
      IceResistDown rec -> writeRecord rec "IceResistDown"
      LightningResistDown rec -> writeRecord rec "LightningResistDown"
      EarthResistDown rec -> writeRecord rec "EarthResistDown"
      WaterResistDown rec -> writeRecord rec "WaterResistDown"
      WindResistDown rec -> writeRecord rec "WindResistDown"
      FireWeakness rec -> writeRecord rec "FireWeakness"
      IceWeakness rec -> writeRecord rec "IceWeakness"
      LightningWeakness rec -> writeRecord rec "LightningWeakness"
      EarthWeakness rec -> writeRecord rec "EarthWeakness"
      WaterWeakness rec -> writeRecord rec "WaterWeakness"
      WindWeakness rec -> writeRecord rec "WindWeakness"
      Enfeeble rec -> writeRecord rec "Enfeeble"
      Stop rec -> writeRecord rec "Stop"
      ExploitWeakness rec -> writeRecord rec "ExploitWeakness"
      HPGain rec -> writeRecord rec "HPGain"
      EnhanceBuffs rec -> writeRecord rec "EnhanceBuffs"
      EnhanceDebuffs rec -> writeRecord rec "EnhanceDebuffs"
      Enliven rec -> writeRecord rec "Enliven"
    where
    writeRecord :: forall rec. WriteForeign rec => rec -> String -> Foreign
    writeRecord rec recType = writeImpl
      { type: recType
      , value: rec
      }

instance WriteForeign Potency where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign FilterEffectType where
  writeImpl = case _ of
    FilterHeal -> writeImpl "FilterHeal"
    FilterVeil -> writeImpl "FilterVeil"
    FilterProvoke -> writeImpl "FilterProvoke"
    FilterEnfeeble -> writeImpl "FilterEnfeeble"
    FilterStop -> writeImpl "FilterStop"
    FilterExploitWeakness -> writeImpl "FilterExploitWeakness"
    FilterHPGain -> writeImpl "FilterHPGain"
    FilterEnhanceBuffs -> writeImpl "FilterEnhanceBuffs"
    FilterEnhanceDebuffs -> writeImpl "FilterEnhanceDebuffs"
    FilterEnliven -> writeImpl "FilterEnliven"
    FilterPatkUp -> writeImpl "FilterPatkUp"
    FilterMatkUp -> writeImpl "FilterMatkUp"
    FilterPdefUp -> writeImpl "FilterPdefUp"
    FilterMdefUp -> writeImpl "FilterMdefUp"
    FilterPhysicalWeaponBoost -> writeImpl "FilterPhysicalWeaponBoost"
    FilterMagicWeaponBoost -> writeImpl "FilterMagicWeaponBoost"
    FilterPhysicalDamageBonus -> writeImpl "FilterPhysicalDamageBonus"
    FilterMagicDamageBonus -> writeImpl "FilterMagicDamageBonus"
    FilterFireDamageUp -> writeImpl "FilterFireDamageUp"
    FilterIceDamageUp -> writeImpl "FilterIceDamageUp"
    FilterLightningDamageUp -> writeImpl "FilterLightningDamageUp"
    FilterEarthDamageUp -> writeImpl "FilterEarthDamageUp"
    FilterWaterDamageUp -> writeImpl "FilterWaterDamageUp"
    FilterWindDamageUp -> writeImpl "FilterWindDamageUp"
    FilterFireResistUp -> writeImpl "FilterFireResistUp"
    FilterIceResistUp -> writeImpl "FilterIceResistUp"
    FilterLightningResistUp -> writeImpl "FilterLightningResistUp"
    FilterEarthResistUp -> writeImpl "FilterEarthResistUp"
    FilterWaterResistUp -> writeImpl "FilterWaterResistUp"
    FilterWindResistUp -> writeImpl "FilterWindResistUp"
    FilterFireWeaponBoost -> writeImpl "FilterFireWeaponBoost"
    FilterIceWeaponBoost -> writeImpl "FilterIceWeaponBoost"
    FilterLightningWeaponBoost -> writeImpl "FilterLightningWeaponBoost"
    FilterEarthWeaponBoost -> writeImpl "FilterEarthWeaponBoost"
    FilterWaterWeaponBoost -> writeImpl "FilterWaterWeaponBoost"
    FilterWindWeaponBoost -> writeImpl "FilterWindWeaponBoost"
    FilterFireDamageBonus -> writeImpl "FilterFireDamageBonus"
    FilterIceDamageBonus -> writeImpl "FilterIceDamageBonus"
    FilterLightningDamageBonus -> writeImpl "FilterLightningDamageBonus"
    FilterEarthDamageBonus -> writeImpl "FilterEarthDamageBonus"
    FilterWaterDamageBonus -> writeImpl "FilterWaterDamageBonus"
    FilterWindDamageBonus -> writeImpl "FilterWindDamageBonus"
    FilterPatkDown -> writeImpl "FilterPatkDown"
    FilterMatkDown -> writeImpl "FilterMatkDown"
    FilterPdefDown -> writeImpl "FilterPdefDown"
    FilterMdefDown -> writeImpl "FilterMdefDown"
    FilterFireDamageDown -> writeImpl "FilterFireDamageDown"
    FilterIceDamageDown -> writeImpl "FilterIceDamageDown"
    FilterLightningDamageDown -> writeImpl "FilterLightningDamageDown"
    FilterEarthDamageDown -> writeImpl "FilterEarthDamageDown"
    FilterWaterDamageDown -> writeImpl "FilterWaterDamageDown"
    FilterWindDamageDown -> writeImpl "FilterWindDamageDown"
    FilterFireResistDown -> writeImpl "FilterFireResistDown"
    FilterIceResistDown -> writeImpl "FilterIceResistDown"
    FilterLightningResistDown -> writeImpl "FilterLightningResistDown"
    FilterEarthResistDown -> writeImpl "FilterEarthResistDown"
    FilterWaterResistDown -> writeImpl "FilterWaterResistDown"
    FilterWindResistDown -> writeImpl "FilterWindResistDown"
    FilterFireWeakness -> writeImpl "FilterFireWeakness"
    FilterIceWeakness -> writeImpl "FilterIceWeakness"
    FilterLightningWeakness -> writeImpl "FilterLightningWeakness"
    FilterEarthWeakness -> writeImpl "FilterEarthWeakness"
    FilterWaterWeakness -> writeImpl "FilterWaterWeakness"
    FilterWindWeakness -> writeImpl "FilterWindWeakness"
    FilterSigilBoostO -> writeImpl "FilterSigilBoostO"
    FilterSigilBoostX -> writeImpl "FilterSigilBoostX"
    FilterSigilBoostTriangle -> writeImpl "FilterSigilBoostTriangle"
    FilterSigilDiamond -> writeImpl "FilterSigilDiamond"

instance WriteForeign Sigil where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

derive newtype instance WriteForeign Percentage
derive newtype instance WriteForeign Duration
derive newtype instance WriteForeign Extension
derive newtype instance WriteForeign CharacterName

instance ReadForeign Range where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign WeaponEffect where
  readImpl json = do
    { type: recType, value } :: { type :: String, value :: Foreign } <- readImpl json
    oneOf' (unexpected recType) $
      exhaustiveWeaponEffectMatch <#> \x -> case x of
        Heal _ -> tryRead Heal recType value "Heal"
        Veil _ -> tryRead Veil recType value "Veil"
        Provoke _ -> tryRead Provoke recType value "Provoke"
        Enfeeble _ -> tryRead Enfeeble recType value "Enfeeble"
        Stop _ -> tryRead Stop recType value "Stop"
        ExploitWeakness _ -> tryRead ExploitWeakness recType value "ExploitWeakness"
        HPGain _ -> tryRead HPGain recType value "HPGain"
        EnhanceBuffs _ -> tryRead EnhanceBuffs recType value "EnhanceBuffs"
        EnhanceDebuffs _ -> tryRead EnhanceDebuffs recType value "EnhanceDebuffs"
        Enliven _ -> tryRead Enliven recType value "Enliven"
        PatkUp _ -> tryRead PatkUp recType value "PatkUp"
        MatkUp _ -> tryRead MatkUp recType value "MatkUp"
        PdefUp _ -> tryRead PdefUp recType value "PdefUp"
        MdefUp _ -> tryRead MdefUp recType value "MdefUp"
        PhysicalWeaponBoost _ -> tryRead PhysicalWeaponBoost recType value "PhysicalWeaponBoost"
        MagicWeaponBoost _ -> tryRead MagicWeaponBoost recType value "MagicWeaponBoost"
        PhysicalDamageBonus _ -> tryRead PhysicalDamageBonus recType value "PhysicalDamageBonus"
        MagicDamageBonus _ -> tryRead MagicDamageBonus recType value "MagicDamageBonus"
        FireDamageUp _ -> tryRead FireDamageUp recType value "FireDamageUp"
        IceDamageUp _ -> tryRead IceDamageUp recType value "IceDamageUp"
        LightningDamageUp _ -> tryRead LightningDamageUp recType value "LightningDamageUp"
        EarthDamageUp _ -> tryRead EarthDamageUp recType value "EarthDamageUp"
        WaterDamageUp _ -> tryRead WaterDamageUp recType value "WaterDamageUp"
        WindDamageUp _ -> tryRead WindDamageUp recType value "WindDamageUp"
        FireResistUp _ -> tryRead FireResistUp recType value "FireResistUp"
        IceResistUp _ -> tryRead IceResistUp recType value "IceResistUp"
        LightningResistUp _ -> tryRead LightningResistUp recType value "LightningResistUp"
        EarthResistUp _ -> tryRead EarthResistUp recType value "EarthResistUp"
        WaterResistUp _ -> tryRead WaterResistUp recType value "WaterResistUp"
        WindResistUp _ -> tryRead WindResistUp recType value "WindResistUp"
        FireWeaponBoost _ -> tryRead FireWeaponBoost recType value "FireWeaponBoost"
        IceWeaponBoost _ -> tryRead IceWeaponBoost recType value "IceWeaponBoost"
        LightningWeaponBoost _ -> tryRead LightningWeaponBoost recType value "LightningWeaponBoost"
        EarthWeaponBoost _ -> tryRead EarthWeaponBoost recType value "EarthWeaponBoost"
        WaterWeaponBoost _ -> tryRead WaterWeaponBoost recType value "WaterWeaponBoost"
        WindWeaponBoost _ -> tryRead WindWeaponBoost recType value "WindWeaponBoost"
        FireDamageBonus _ -> tryRead FireDamageBonus recType value "FireDamageBonus"
        IceDamageBonus _ -> tryRead IceDamageBonus recType value "IceDamageBonus"
        LightningDamageBonus _ -> tryRead LightningDamageBonus recType value "LightningDamageBonus"
        EarthDamageBonus _ -> tryRead EarthDamageBonus recType value "EarthDamageBonus"
        WaterDamageBonus _ -> tryRead WaterDamageBonus recType value "WaterDamageBonus"
        WindDamageBonus _ -> tryRead WindDamageBonus recType value "WindDamageBonus"
        PatkDown _ -> tryRead PatkDown recType value "PatkDown"
        MatkDown _ -> tryRead MatkDown recType value "MatkDown"
        PdefDown _ -> tryRead PdefDown recType value "PdefDown"
        MdefDown _ -> tryRead MdefDown recType value "MdefDown"
        FireDamageDown _ -> tryRead FireDamageDown recType value "FireDamageDown"
        IceDamageDown _ -> tryRead IceDamageDown recType value "IceDamageDown"
        LightningDamageDown _ -> tryRead LightningDamageDown recType value "LightningDamageDown"
        EarthDamageDown _ -> tryRead EarthDamageDown recType value "EarthDamageDown"
        WaterDamageDown _ -> tryRead WaterDamageDown recType value "WaterDamageDown"
        WindDamageDown _ -> tryRead WindDamageDown recType value "WindDamageDown"
        FireResistDown _ -> tryRead FireResistDown recType value "FireResistDown"
        IceResistDown _ -> tryRead IceResistDown recType value "IceResistDown"
        LightningResistDown _ -> tryRead LightningResistDown recType value "LightningResistDown"
        EarthResistDown _ -> tryRead EarthResistDown recType value "EarthResistDown"
        WaterResistDown _ -> tryRead WaterResistDown recType value "WaterResistDown"
        WindResistDown _ -> tryRead WindResistDown recType value "WindResistDown"
        FireWeakness _ -> tryRead FireWeakness recType value "FireWeakness"
        IceWeakness _ -> tryRead IceWeakness recType value "IceWeakness"
        LightningWeakness _ -> tryRead LightningWeakness recType value "LightningWeakness"
        EarthWeakness _ -> tryRead EarthWeakness recType value "EarthWeakness"
        WaterWeakness _ -> tryRead WaterWeakness recType value "WaterWeakness"
        WindWeakness _ -> tryRead WindWeakness recType value "WindWeakness"
    where
    tryRead :: forall rec. ReadForeign rec => (rec -> WeaponEffect) -> String -> Foreign -> String -> Foreign.F WeaponEffect
    tryRead constructor recType value expectedTag =
      if recType == expectedTag then do
        rec :: rec <- readImpl value
        pure $ constructor rec
      else unexpected recType

    unexpected :: forall a. String -> Foreign.F a
    unexpected str = Foreign.fail $ Foreign.ForeignError $ "Unexpected FilterEffectType: " <> str

instance ReadForeign Potency where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign FilterEffectType where
  readImpl json = do
    str :: String <- readImpl json
    oneOf' (unexpected str) $
      allFilterEffectTypes <#> \x -> case x of
        FilterHeal -> tryRead x str "FilterHeal"
        FilterVeil -> tryRead x str "FilterVeil"
        FilterProvoke -> tryRead x str "FilterProvoke"
        FilterEnfeeble -> tryRead x str "FilterEnfeeble"
        FilterStop -> tryRead x str "FilterStop"
        FilterExploitWeakness -> tryRead x str "FilterExploitWeakness"
        FilterHPGain -> tryRead x str "FilterHPGain"
        FilterEnhanceBuffs -> tryRead x str "FilterEnhanceBuffs"
        FilterEnhanceDebuffs -> tryRead x str "FilterEnhanceDebuffs"
        FilterEnliven -> tryRead x str "FilterEnliven"
        FilterPatkUp -> tryRead x str "FilterPatkUp"
        FilterMatkUp -> tryRead x str "FilterMatkUp"
        FilterPdefUp -> tryRead x str "FilterPdefUp"
        FilterMdefUp -> tryRead x str "FilterMdefUp"
        FilterPhysicalWeaponBoost -> tryRead x str "FilterPhysicalWeaponBoost"
        FilterMagicWeaponBoost -> tryRead x str "FilterMagicWeaponBoost"
        FilterPhysicalDamageBonus -> tryRead x str "FilterPhysicalDamageBonus"
        FilterMagicDamageBonus -> tryRead x str "FilterMagicDamageBonus"
        FilterFireDamageUp -> tryRead x str "FilterFireDamageUp"
        FilterIceDamageUp -> tryRead x str "FilterIceDamageUp"
        FilterLightningDamageUp -> tryRead x str "FilterLightningDamageUp"
        FilterEarthDamageUp -> tryRead x str "FilterEarthDamageUp"
        FilterWaterDamageUp -> tryRead x str "FilterWaterDamageUp"
        FilterWindDamageUp -> tryRead x str "FilterWindDamageUp"
        FilterFireResistUp -> tryRead x str "FilterFireResistUp"
        FilterIceResistUp -> tryRead x str "FilterIceResistUp"
        FilterLightningResistUp -> tryRead x str "FilterLightningResistUp"
        FilterEarthResistUp -> tryRead x str "FilterEarthResistUp"
        FilterWaterResistUp -> tryRead x str "FilterWaterResistUp"
        FilterWindResistUp -> tryRead x str "FilterWindResistUp"
        FilterFireWeaponBoost -> tryRead x str "FilterFireWeaponBoost"
        FilterIceWeaponBoost -> tryRead x str "FilterIceWeaponBoost"
        FilterLightningWeaponBoost -> tryRead x str "FilterLightningWeaponBoost"
        FilterEarthWeaponBoost -> tryRead x str "FilterEarthWeaponBoost"
        FilterWaterWeaponBoost -> tryRead x str "FilterWaterWeaponBoost"
        FilterWindWeaponBoost -> tryRead x str "FilterWindWeaponBoost"
        FilterFireDamageBonus -> tryRead x str "FilterFireDamageBonus"
        FilterIceDamageBonus -> tryRead x str "FilterIceDamageBonus"
        FilterLightningDamageBonus -> tryRead x str "FilterLightningDamageBonus"
        FilterEarthDamageBonus -> tryRead x str "FilterEarthDamageBonus"
        FilterWaterDamageBonus -> tryRead x str "FilterWaterDamageBonus"
        FilterWindDamageBonus -> tryRead x str "FilterWindDamageBonus"
        FilterPatkDown -> tryRead x str "FilterPatkDown"
        FilterMatkDown -> tryRead x str "FilterMatkDown"
        FilterPdefDown -> tryRead x str "FilterPdefDown"
        FilterMdefDown -> tryRead x str "FilterMdefDown"
        FilterFireDamageDown -> tryRead x str "FilterFireDamageDown"
        FilterIceDamageDown -> tryRead x str "FilterIceDamageDown"
        FilterLightningDamageDown -> tryRead x str "FilterLightningDamageDown"
        FilterEarthDamageDown -> tryRead x str "FilterEarthDamageDown"
        FilterWaterDamageDown -> tryRead x str "FilterWaterDamageDown"
        FilterWindDamageDown -> tryRead x str "FilterWindDamageDown"
        FilterFireResistDown -> tryRead x str "FilterFireResistDown"
        FilterIceResistDown -> tryRead x str "FilterIceResistDown"
        FilterLightningResistDown -> tryRead x str "FilterLightningResistDown"
        FilterEarthResistDown -> tryRead x str "FilterEarthResistDown"
        FilterWaterResistDown -> tryRead x str "FilterWaterResistDown"
        FilterWindResistDown -> tryRead x str "FilterWindResistDown"
        FilterFireWeakness -> tryRead x str "FilterFireWeakness"
        FilterIceWeakness -> tryRead x str "FilterIceWeakness"
        FilterLightningWeakness -> tryRead x str "FilterLightningWeakness"
        FilterEarthWeakness -> tryRead x str "FilterEarthWeakness"
        FilterWaterWeakness -> tryRead x str "FilterWaterWeakness"
        FilterWindWeakness -> tryRead x str "FilterWindWeakness"
        FilterSigilBoostO -> tryRead x str "FilterSigilBoostO"
        FilterSigilBoostX -> tryRead x str "FilterSigilBoostX"
        FilterSigilBoostTriangle -> tryRead x str "FilterSigilBoostTriangle"
        FilterSigilDiamond -> tryRead x str "FilterSigilDiamond"
    where
    tryRead :: FilterEffectType -> String -> String -> Foreign.F FilterEffectType
    tryRead filterEffectType str expectedTag =
      if str == expectedTag then pure filterEffectType else unexpected str

    unexpected :: forall a. String -> Foreign.F a
    unexpected str = Foreign.fail $ Foreign.ForeignError $ "Unexpected FilterEffectType: " <> str

instance ReadForeign Sigil where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

derive newtype instance ReadForeign Percentage
derive newtype instance ReadForeign Duration
derive newtype instance ReadForeign Extension
derive newtype instance ReadForeign CharacterName

instance Display CharacterName where
  display = display <<< unwrap

-- | NOTE: this is how filters are displayed in the UI.
instance Display FilterEffectType where
  display = case _ of
    FilterHeal -> "Heal"

    FilterVeil -> "Veil"
    FilterProvoke -> "Provoke"
    FilterEnfeeble -> "Enfeeble"
    FilterStop -> "Stop"
    FilterExploitWeakness -> "Exploit Weakness"
    FilterHPGain -> "HP Gain"
    FilterEnhanceBuffs -> "Enhance Buffs"
    FilterEnhanceDebuffs -> "Enhance Debuffs"
    FilterEnliven -> "Enliven"

    FilterPatkUp -> "PATK up"
    FilterMatkUp -> "MATK up"
    FilterPdefUp -> "PDEF up"
    FilterMdefUp -> "MDEF up"
    FilterPhysicalWeaponBoost -> "Phys. weapon boost"
    FilterMagicWeaponBoost -> "Mag. weapon boost"
    FilterPhysicalDamageBonus -> "Phys. damage bonus"
    FilterMagicDamageBonus -> "Mag. damage bonus"
    FilterFireDamageUp -> "Fire damage up"
    FilterIceDamageUp -> "Ice damage up"
    FilterLightningDamageUp -> "Lightning damage up"
    FilterEarthDamageUp -> "Earth damage up"
    FilterWaterDamageUp -> "Water damage up"
    FilterWindDamageUp -> "Wind damage up"
    FilterFireResistUp -> "Fire resist up"
    FilterIceResistUp -> "Ice resist up"
    FilterLightningResistUp -> "Lightning resist up"
    FilterEarthResistUp -> "Earth resist up"
    FilterWaterResistUp -> "Water resist up"
    FilterWindResistUp -> "Wind resist up"
    FilterFireWeaponBoost -> "Fire weapon boost"
    FilterIceWeaponBoost -> "Ice weapon boost"
    FilterLightningWeaponBoost -> "Lightning weapon boost"
    FilterEarthWeaponBoost -> "Earth weapon boost"
    FilterWaterWeaponBoost -> "Water weapon boost"
    FilterWindWeaponBoost -> "Wind weapon boost"
    FilterFireDamageBonus -> "Fire damage bonus"
    FilterIceDamageBonus -> "Ice damage bonus"
    FilterLightningDamageBonus -> "Lightning damage bonus"
    FilterEarthDamageBonus -> "Earth damage bonus"
    FilterWaterDamageBonus -> "Water damage bonus"
    FilterWindDamageBonus -> "Wind damage bonus"

    FilterPatkDown -> "PATK down"
    FilterMatkDown -> "MATK down"
    FilterPdefDown -> "PDEF down"
    FilterMdefDown -> "MDEF down"
    FilterFireDamageDown -> "Fire damage down"
    FilterIceDamageDown -> "Ice damage down"
    FilterLightningDamageDown -> "Lightning damage down"
    FilterEarthDamageDown -> "Earth damage down"
    FilterWaterDamageDown -> "Water damage down"
    FilterWindDamageDown -> "Wind damage down"
    FilterFireResistDown -> "Fire resist down"
    FilterIceResistDown -> "Ice resist down"
    FilterLightningResistDown -> "Lightning resist down"
    FilterEarthResistDown -> "Earth resist down"
    FilterWaterResistDown -> "Water resist down"
    FilterWindResistDown -> "Wind resist down"
    FilterFireWeakness -> "Fire weakness"
    FilterIceWeakness -> "Ice weakness"
    FilterLightningWeakness -> "Lightning weakness"
    FilterEarthWeakness -> "Earth weakness"
    FilterWaterWeakness -> "Water weakness"
    FilterWindWeakness -> "Wind weakness"

    FilterSigilBoostO -> "⏺ Sigil Boost"
    FilterSigilBoostX -> "✖ Sigil Boost"
    FilterSigilBoostTriangle -> "▲ Sigil Boost"
    FilterSigilDiamond -> "◆ Sigil"

allPossiblePotencies :: Array Potency
allPossiblePotencies = Utils.listEnum

allFilterEffectTypes :: Array FilterEffectType
allFilterEffectTypes =
  [ FilterHeal
  , FilterVeil
  , FilterProvoke
  , FilterEnfeeble
  , FilterStop
  , FilterExploitWeakness
  , FilterHPGain
  , FilterEnhanceBuffs
  , FilterEnhanceDebuffs
  , FilterEnliven
  , FilterPatkUp
  , FilterMatkUp
  , FilterPdefUp
  , FilterMdefUp
  , FilterPhysicalWeaponBoost
  , FilterMagicWeaponBoost
  , FilterPhysicalDamageBonus
  , FilterMagicDamageBonus
  , FilterFireDamageUp
  , FilterIceDamageUp
  , FilterLightningDamageUp
  , FilterEarthDamageUp
  , FilterWaterDamageUp
  , FilterWindDamageUp
  , FilterFireResistUp
  , FilterIceResistUp
  , FilterLightningResistUp
  , FilterEarthResistUp
  , FilterWaterResistUp
  , FilterWindResistUp
  , FilterFireWeaponBoost
  , FilterIceWeaponBoost
  , FilterLightningWeaponBoost
  , FilterEarthWeaponBoost
  , FilterWaterWeaponBoost
  , FilterWindWeaponBoost
  , FilterFireDamageBonus
  , FilterIceDamageBonus
  , FilterLightningDamageBonus
  , FilterEarthDamageBonus
  , FilterWaterDamageBonus
  , FilterWindDamageBonus
  , FilterPatkDown
  , FilterMatkDown
  , FilterPdefDown
  , FilterMdefDown
  , FilterFireDamageDown
  , FilterIceDamageDown
  , FilterLightningDamageDown
  , FilterEarthDamageDown
  , FilterWaterDamageDown
  , FilterWindDamageDown
  , FilterFireResistDown
  , FilterIceResistDown
  , FilterLightningResistDown
  , FilterEarthResistDown
  , FilterWaterResistDown
  , FilterWindResistDown
  , FilterFireWeakness
  , FilterIceWeakness
  , FilterLightningWeakness
  , FilterEarthWeakness
  , FilterWaterWeakness
  , FilterWindWeakness
  , FilterSigilBoostO
  , FilterSigilBoostX
  , FilterSigilBoostTriangle
  , FilterSigilDiamond
  ]
  where
  _ = case _ of
    -- Unfortunately, there's no way to ensure our returns values are exhaustive
    -- https://alistairb.dev/exhaustive-return-warning/
    --
    -- As a workaround, we have this useless pattern match here to remind us to update the list above when new variants are added.
    FilterHeal -> unit
    FilterVeil -> unit
    FilterProvoke -> unit
    FilterEnfeeble -> unit
    FilterStop -> unit
    FilterExploitWeakness -> unit
    FilterHPGain -> unit
    FilterEnhanceBuffs -> unit
    FilterEnhanceDebuffs -> unit
    FilterEnliven -> unit
    FilterPatkUp -> unit
    FilterMatkUp -> unit
    FilterPdefUp -> unit
    FilterMdefUp -> unit
    FilterPhysicalWeaponBoost -> unit
    FilterMagicWeaponBoost -> unit
    FilterPhysicalDamageBonus -> unit
    FilterMagicDamageBonus -> unit
    FilterFireDamageUp -> unit
    FilterIceDamageUp -> unit
    FilterLightningDamageUp -> unit
    FilterEarthDamageUp -> unit
    FilterWaterDamageUp -> unit
    FilterWindDamageUp -> unit
    FilterFireResistUp -> unit
    FilterIceResistUp -> unit
    FilterLightningResistUp -> unit
    FilterEarthResistUp -> unit
    FilterWaterResistUp -> unit
    FilterWindResistUp -> unit
    FilterFireWeaponBoost -> unit
    FilterIceWeaponBoost -> unit
    FilterLightningWeaponBoost -> unit
    FilterEarthWeaponBoost -> unit
    FilterWaterWeaponBoost -> unit
    FilterWindWeaponBoost -> unit
    FilterFireDamageBonus -> unit
    FilterIceDamageBonus -> unit
    FilterLightningDamageBonus -> unit
    FilterEarthDamageBonus -> unit
    FilterWaterDamageBonus -> unit
    FilterWindDamageBonus -> unit
    FilterPatkDown -> unit
    FilterMatkDown -> unit
    FilterPdefDown -> unit
    FilterMdefDown -> unit
    FilterFireDamageDown -> unit
    FilterIceDamageDown -> unit
    FilterLightningDamageDown -> unit
    FilterEarthDamageDown -> unit
    FilterWaterDamageDown -> unit
    FilterWindDamageDown -> unit
    FilterFireResistDown -> unit
    FilterIceResistDown -> unit
    FilterLightningResistDown -> unit
    FilterEarthResistDown -> unit
    FilterWaterResistDown -> unit
    FilterWindResistDown -> unit
    FilterFireWeakness -> unit
    FilterIceWeakness -> unit
    FilterLightningWeakness -> unit
    FilterEarthWeakness -> unit
    FilterWaterWeakness -> unit
    FilterWindWeakness -> unit
    FilterSigilBoostO -> unit
    FilterSigilBoostX -> unit
    FilterSigilBoostTriangle -> unit
    -- Unfortunately, there's no way to ensure our returns values are exhaustive
    -- https://alistairb.dev/exhaustive-return-warning/
    --
    -- As a workaround, we have this useless pattern match here to remind us to update the list above when new variants are added.
    FilterSigilDiamond -> unit

exhaustiveWeaponEffectMatch :: Array WeaponEffect
exhaustiveWeaponEffectMatch =
  [ Heal { range, percentage }
  , PatkUp { range, durExt, potencies }
  , MatkUp { range, durExt, potencies }
  , PdefUp { range, durExt, potencies }
  , MdefUp { range, durExt, potencies }
  , PhysicalWeaponBoost { range, durExt, percentage }
  , MagicWeaponBoost { range, durExt, percentage }
  , PhysicalDamageBonus { range, durExt, percentage }
  , MagicDamageBonus { range, durExt, percentage }
  , FireDamageUp { range, durExt, potencies }
  , IceDamageUp { range, durExt, potencies }
  , LightningDamageUp { range, durExt, potencies }
  , EarthDamageUp { range, durExt, potencies }
  , WaterDamageUp { range, durExt, potencies }
  , WindDamageUp { range, durExt, potencies }
  , FireResistUp { range, durExt, potencies }
  , IceResistUp { range, durExt, potencies }
  , LightningResistUp { range, durExt, potencies }
  , EarthResistUp { range, durExt, potencies }
  , WaterResistUp { range, durExt, potencies }
  , WindResistUp { range, durExt, potencies }
  , FireWeaponBoost { range, durExt, percentage }
  , IceWeaponBoost { range, durExt, percentage }
  , LightningWeaponBoost { range, durExt, percentage }
  , EarthWeaponBoost { range, durExt, percentage }
  , WaterWeaponBoost { range, durExt, percentage }
  , WindWeaponBoost { range, durExt, percentage }
  , FireDamageBonus { range, durExt, percentage }
  , IceDamageBonus { range, durExt, percentage }
  , LightningDamageBonus { range, durExt, percentage }
  , EarthDamageBonus { range, durExt, percentage }
  , WaterDamageBonus { range, durExt, percentage }
  , WindDamageBonus { range, durExt, percentage }
  , Veil { range, durExt, percentage }
  , Provoke { range, durExt }
  , PatkDown { range, durExt, potencies }
  , MatkDown { range, durExt, potencies }
  , PdefDown { range, durExt, potencies }
  , MdefDown { range, durExt, potencies }



  , FireDamageDown { range, durExt, potencies }
  , IceDamageDown { range, durExt, potencies }
  , LightningDamageDown { range, durExt, potencies }
  , EarthDamageDown { range, durExt, potencies }
  , WaterDamageDown { range, durExt, potencies }
  , WindDamageDown { range, durExt, potencies }
  , FireResistDown { range, durExt, potencies }
  , IceResistDown { range, durExt, potencies }
  , LightningResistDown { range, durExt, potencies }
  , EarthResistDown { range, durExt, potencies }
  , WaterResistDown { range, durExt, potencies }
  , WindResistDown { range, durExt, potencies }
  , FireWeakness { range, durExt, percentage }
  , IceWeakness { range, durExt, percentage }
  , LightningWeakness { range, durExt, percentage }
  , EarthWeakness { range, durExt, percentage }
  , WaterWeakness { range, durExt, percentage }
  , WindWeakness { range, durExt, percentage }
  , Enfeeble { range, durExt }
  , Stop { range, durExt }
  , ExploitWeakness { range, durExt, percentage }
  , HPGain { range, durExt, percentage }
  , EnhanceBuffs { range, durExt, potencies }
  , EnhanceDebuffs { range, durExt, potencies }
  , Enliven { range, durExt }
  ]
  where
  range = All
  durExt = { duration: Duration 0, extension: Extension 0 }
  percentage = Percentage 0
  potencies = { base: Low, max: Low }

  _ = case _ of
    -- Unfortunately, there's no way to ensure our returns values are exhaustive
    -- https://alistairb.dev/exhaustive-return-warning/
    --
    -- As a workaround, we have this useless pattern match here to remind us to update the list above when new variants are added.
    Heal _ -> unit
    PatkUp _ -> unit
    MatkUp _ -> unit
    PdefUp _ -> unit
    MdefUp _ -> unit
    FireDamageUp _ -> unit
    IceDamageUp _ -> unit
    LightningDamageUp _ -> unit
    EarthDamageUp _ -> unit
    WaterDamageUp _ -> unit
    WindDamageUp _ -> unit
    FireResistUp _ -> unit
    IceResistUp _ -> unit
    LightningResistUp _ -> unit
    EarthResistUp _ -> unit
    WaterResistUp _ -> unit
    WindResistUp _ -> unit
    Veil _ -> unit
    Provoke _ -> unit
    PatkDown _ -> unit
    MatkDown _ -> unit
    PdefDown _ -> unit
    MdefDown _ -> unit
    PhysicalWeaponBoost _ -> unit
    MagicWeaponBoost _ -> unit
    PhysicalDamageBonus _ -> unit
    MagicDamageBonus _ -> unit
    FireWeaponBoost _ -> unit
    IceWeaponBoost _ -> unit
    LightningWeaponBoost _ -> unit
    EarthWeaponBoost _ -> unit
    WaterWeaponBoost _ -> unit
    WindWeaponBoost _ -> unit
    FireDamageBonus _ -> unit
    IceDamageBonus _ -> unit
    LightningDamageBonus _ -> unit
    EarthDamageBonus _ -> unit
    WaterDamageBonus _ -> unit
    WindDamageBonus _ -> unit
    FireDamageDown _ -> unit
    IceDamageDown _ -> unit
    LightningDamageDown _ -> unit
    EarthDamageDown _ -> unit
    WaterDamageDown _ -> unit
    WindDamageDown _ -> unit
    FireResistDown _ -> unit
    IceResistDown _ -> unit
    LightningResistDown _ -> unit
    EarthResistDown _ -> unit
    WaterResistDown _ -> unit
    WindResistDown _ -> unit
    FireWeakness _ -> unit
    IceWeakness _ -> unit
    LightningWeakness _ -> unit
    EarthWeakness _ -> unit
    WaterWeakness _ -> unit
    WindWeakness _ -> unit
    Enfeeble _ -> unit
    Stop _ -> unit
    ExploitWeakness _ -> unit
    HPGain _ -> unit
    EnhanceBuffs _ -> unit
    EnhanceDebuffs _ -> unit
    -- Unfortunately, there's no way to ensure our returns values are exhaustive
    -- https://alistairb.dev/exhaustive-return-warning/
    --
    -- As a workaround, we have this useless pattern match here to remind us to update the list above when new variants are added.
    Enliven _ -> unit

instance Display Potency where
  display =
    case _ of
      Low -> "Low"
      Mid -> "Mid"
      High -> "High"
      ExtraHigh -> "Extra High"
