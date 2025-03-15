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
  | FireWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | IceWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | ThunderWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | EarthWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | WaterWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
  | WindWeakness { range :: Range, durExt :: DurExt, percentage :: Percentage }
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
  | FilterFireWeakness
  | FilterIceWeakness
  | FilterThunderWeakness
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
      FireDamageUp rec -> showRec rec "FireDamageUp"
      IceDamageUp rec -> showRec rec "IceDamageUp"
      ThunderDamageUp rec -> showRec rec "ThunderDamageUp"
      EarthDamageUp rec -> showRec rec "EarthDamageUp"
      WaterDamageUp rec -> showRec rec "WaterDamageUp"
      WindDamageUp rec -> showRec rec "WindDamageUp"
      Veil rec -> showRec rec "Veil"
      Provoke rec -> showRec rec "Provoke"
      PatkDown rec -> showRec rec "PatkDown"
      MatkDown rec -> showRec rec "MatkDown"
      PdefDown rec -> showRec rec "PdefDown"
      MdefDown rec -> showRec rec "MdefDown"
      FireResistDown rec -> showRec rec "FireResistDown"
      IceResistDown rec -> showRec rec "IceResistDown"
      ThunderResistDown rec -> showRec rec "ThunderResistDown"
      EarthResistDown rec -> showRec rec "EarthResistDown"
      WaterResistDown rec -> showRec rec "WaterResistDown"
      WindResistDown rec -> showRec rec "WindResistDown"
      FireWeakness rec -> showRec rec "FireWeakness"
      IceWeakness rec -> showRec rec "IceWeakness"
      ThunderWeakness rec -> showRec rec "ThunderWeakness"
      EarthWeakness rec -> showRec rec "EarthWeakness"
      WaterWeakness rec -> showRec rec "WaterWeakness"
      WindWeakness rec -> showRec rec "WindWeakness"
      Enfeeble rec -> showRec rec "Enfeeble"
      Stop rec -> showRec rec "Stop"
      ExploitWeakness rec -> showRec rec "ExploitWeakness"
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
    FilterPatkUp -> "FilterPatkUp"
    FilterMatkUp -> "FilterMatkUp"
    FilterPdefUp -> "FilterPdefUp"
    FilterMdefUp -> "FilterMdefUp"
    FilterFireDamageUp -> "FilterFireDamageUp"
    FilterIceDamageUp -> "FilterIceDamageUp"
    FilterThunderDamageUp -> "FilterThunderDamageUp"
    FilterEarthDamageUp -> "FilterEarthDamageUp"
    FilterWaterDamageUp -> "FilterWaterDamageUp"
    FilterWindDamageUp -> "FilterWindDamageUp"
    FilterPatkDown -> "FilterPatkDown"
    FilterMatkDown -> "FilterMatkDown"
    FilterPdefDown -> "FilterPdefDown"
    FilterMdefDown -> "FilterMdefDown"
    FilterFireResistDown -> "FilterFireResistDown"
    FilterIceResistDown -> "FilterIceResistDown"
    FilterThunderResistDown -> "FilterThunderResistDown"
    FilterEarthResistDown -> "FilterEarthResistDown"
    FilterWaterResistDown -> "FilterWaterResistDown"
    FilterWindResistDown -> "FilterWindResistDown"
    FilterFireWeakness -> "FilterFireWeakness"
    FilterIceWeakness -> "FilterIceWeakness"
    FilterThunderWeakness -> "FilterThunderWeakness"
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
      FireDamageUp rec -> writeRecord rec "FireDamageUp"
      IceDamageUp rec -> writeRecord rec "IceDamageUp"
      ThunderDamageUp rec -> writeRecord rec "ThunderDamageUp"
      EarthDamageUp rec -> writeRecord rec "EarthDamageUp"
      WaterDamageUp rec -> writeRecord rec "WaterDamageUp"
      WindDamageUp rec -> writeRecord rec "WindDamageUp"
      Veil rec -> writeRecord rec "Veil"
      Provoke rec -> writeRecord rec "Provoke"
      PatkDown rec -> writeRecord rec "PatkDown"
      MatkDown rec -> writeRecord rec "MatkDown"
      PdefDown rec -> writeRecord rec "PdefDown"
      MdefDown rec -> writeRecord rec "MdefDown"
      FireResistDown rec -> writeRecord rec "FireResistDown"
      IceResistDown rec -> writeRecord rec "IceResistDown"
      ThunderResistDown rec -> writeRecord rec "ThunderResistDown"
      EarthResistDown rec -> writeRecord rec "EarthResistDown"
      WaterResistDown rec -> writeRecord rec "WaterResistDown"
      WindResistDown rec -> writeRecord rec "WindResistDown"
      FireWeakness rec -> writeRecord rec "FireWeakness"
      IceWeakness rec -> writeRecord rec "IceWeakness"
      ThunderWeakness rec -> writeRecord rec "ThunderWeakness"
      EarthWeakness rec -> writeRecord rec "EarthWeakness"
      WaterWeakness rec -> writeRecord rec "WaterWeakness"
      WindWeakness rec -> writeRecord rec "WindWeakness"
      Enfeeble rec -> writeRecord rec "Enfeeble"
      Stop rec -> writeRecord rec "Stop"
      ExploitWeakness rec -> writeRecord rec "ExploitWeakness"
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
    FilterPatkUp -> writeImpl "FilterPatkUp"
    FilterMatkUp -> writeImpl "FilterMatkUp"
    FilterPdefUp -> writeImpl "FilterPdefUp"
    FilterMdefUp -> writeImpl "FilterMdefUp"
    FilterFireDamageUp -> writeImpl "FilterFireDamageUp"
    FilterIceDamageUp -> writeImpl "FilterIceDamageUp"
    FilterThunderDamageUp -> writeImpl "FilterThunderDamageUp"
    FilterEarthDamageUp -> writeImpl "FilterEarthDamageUp"
    FilterWaterDamageUp -> writeImpl "FilterWaterDamageUp"
    FilterWindDamageUp -> writeImpl "FilterWindDamageUp"
    FilterPatkDown -> writeImpl "FilterPatkDown"
    FilterMatkDown -> writeImpl "FilterMatkDown"
    FilterPdefDown -> writeImpl "FilterPdefDown"
    FilterMdefDown -> writeImpl "FilterMdefDown"
    FilterFireResistDown -> writeImpl "FilterFireResistDown"
    FilterIceResistDown -> writeImpl "FilterIceResistDown"
    FilterThunderResistDown -> writeImpl "FilterThunderResistDown"
    FilterEarthResistDown -> writeImpl "FilterEarthResistDown"
    FilterWaterResistDown -> writeImpl "FilterWaterResistDown"
    FilterWindResistDown -> writeImpl "FilterWindResistDown"
    FilterFireWeakness -> writeImpl "FilterFireWeakness"
    FilterIceWeakness -> writeImpl "FilterIceWeakness"
    FilterThunderWeakness -> writeImpl "FilterThunderWeakness"
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
    -- str :: String <- readImpl j son
    -- x :: X <- readImpl json
    { type: recType, value } :: { type :: String, value :: Foreign } <- readImpl json
    -- let { type, value } = x
    oneOf' (unexpected recType) $
      exhaustiveWeaponEffectMatch <#> \x -> case x of
        Heal _ -> tryRead Heal recType value "Heal"
        Veil _ -> tryRead Veil recType value "Veil"
        Provoke _ -> tryRead Provoke recType value "Provoke"
        Enfeeble _ -> tryRead Enfeeble recType value "Enfeeble"
        Stop _ -> tryRead Stop recType value "Stop"
        ExploitWeakness _ -> tryRead ExploitWeakness recType value "ExploitWeakness"
        PatkUp _ -> tryRead PatkUp recType value "PatkUp"
        MatkUp _ -> tryRead MatkUp recType value "MatkUp"
        PdefUp _ -> tryRead PdefUp recType value "PdefUp"
        MdefUp _ -> tryRead MdefUp recType value "MdefUp"
        FireDamageUp _ -> tryRead FireDamageUp recType value "FireDamageUp"
        IceDamageUp _ -> tryRead IceDamageUp recType value "IceDamageUp"
        ThunderDamageUp _ -> tryRead ThunderDamageUp recType value "ThunderDamageUp"
        EarthDamageUp _ -> tryRead EarthDamageUp recType value "EarthDamageUp"
        WaterDamageUp _ -> tryRead WaterDamageUp recType value "WaterDamageUp"
        WindDamageUp _ -> tryRead WindDamageUp recType value "WindDamageUp"
        PatkDown _ -> tryRead PatkDown recType value "PatkDown"
        MatkDown _ -> tryRead MatkDown recType value "MatkDown"
        PdefDown _ -> tryRead PdefDown recType value "PdefDown"
        MdefDown _ -> tryRead MdefDown recType value "MdefDown"
        FireResistDown _ -> tryRead FireResistDown recType value "FireResistDown"
        IceResistDown _ -> tryRead IceResistDown recType value "IceResistDown"
        ThunderResistDown _ -> tryRead ThunderResistDown recType value "ThunderResistDown"
        EarthResistDown _ -> tryRead EarthResistDown recType value "EarthResistDown"
        WaterResistDown _ -> tryRead WaterResistDown recType value "WaterResistDown"
        WindResistDown _ -> tryRead WindResistDown recType value "WindResistDown"
        FireWeakness _ -> tryRead FireWeakness recType value "FireWeakness"
        IceWeakness _ -> tryRead IceWeakness recType value "IceWeakness"
        ThunderWeakness _ -> tryRead ThunderWeakness recType value "ThunderWeakness"
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
        FilterPatkUp -> tryRead x str "FilterPatkUp"
        FilterMatkUp -> tryRead x str "FilterMatkUp"
        FilterPdefUp -> tryRead x str "FilterPdefUp"
        FilterMdefUp -> tryRead x str "FilterMdefUp"
        FilterFireDamageUp -> tryRead x str "FilterFireDamageUp"
        FilterIceDamageUp -> tryRead x str "FilterIceDamageUp"
        FilterThunderDamageUp -> tryRead x str "FilterThunderDamageUp"
        FilterEarthDamageUp -> tryRead x str "FilterEarthDamageUp"
        FilterWaterDamageUp -> tryRead x str "FilterWaterDamageUp"
        FilterWindDamageUp -> tryRead x str "FilterWindDamageUp"
        FilterPatkDown -> tryRead x str "FilterPatkDown"
        FilterMatkDown -> tryRead x str "FilterMatkDown"
        FilterPdefDown -> tryRead x str "FilterPdefDown"
        FilterMdefDown -> tryRead x str "FilterMdefDown"
        FilterFireResistDown -> tryRead x str "FilterFireResistDown"
        FilterIceResistDown -> tryRead x str "FilterIceResistDown"
        FilterThunderResistDown -> tryRead x str "FilterThunderResistDown"
        FilterEarthResistDown -> tryRead x str "FilterEarthResistDown"
        FilterWaterResistDown -> tryRead x str "FilterWaterResistDown"
        FilterWindResistDown -> tryRead x str "FilterWindResistDown"
        FilterFireWeakness -> tryRead x str "FilterFireWeakness"
        FilterIceWeakness -> tryRead x str "FilterIceWeakness"
        FilterThunderWeakness -> tryRead x str "FilterThunderWeakness"
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
    FilterFireWeakness -> "Fire weakness"
    FilterIceWeakness -> "Ice weakness"
    FilterThunderWeakness -> "Thunder weakness"
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
  , FilterPatkUp
  , FilterMatkUp
  , FilterPdefUp
  , FilterMdefUp
  , FilterFireDamageUp
  , FilterIceDamageUp
  , FilterThunderDamageUp
  , FilterEarthDamageUp
  , FilterWaterDamageUp
  , FilterWindDamageUp
  , FilterPatkDown
  , FilterMatkDown
  , FilterPdefDown
  , FilterMdefDown
  , FilterFireResistDown
  , FilterIceResistDown
  , FilterThunderResistDown
  , FilterEarthResistDown
  , FilterWaterResistDown
  , FilterWindResistDown
  , FilterFireWeakness
  , FilterIceWeakness
  , FilterThunderWeakness
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
    FilterPatkUp -> unit
    FilterMatkUp -> unit
    FilterPdefUp -> unit
    FilterMdefUp -> unit
    FilterFireDamageUp -> unit
    FilterIceDamageUp -> unit
    FilterThunderDamageUp -> unit
    FilterEarthDamageUp -> unit
    FilterWaterDamageUp -> unit
    FilterWindDamageUp -> unit
    FilterPatkDown -> unit
    FilterMatkDown -> unit
    FilterPdefDown -> unit
    FilterMdefDown -> unit
    FilterFireResistDown -> unit
    FilterIceResistDown -> unit
    FilterThunderResistDown -> unit
    FilterEarthResistDown -> unit
    FilterWaterResistDown -> unit
    FilterWindResistDown -> unit
    FilterFireWeakness -> unit
    FilterIceWeakness -> unit
    FilterThunderWeakness -> unit
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
  , FireDamageUp { range, durExt, potencies }
  , IceDamageUp { range, durExt, potencies }
  , ThunderDamageUp { range, durExt, potencies }
  , EarthDamageUp { range, durExt, potencies }
  , WaterDamageUp { range, durExt, potencies }
  , WindDamageUp { range, durExt, potencies }
  , Veil { range, durExt, percentage }
  , Provoke { range, durExt }
  , PatkDown { range, durExt, potencies }
  , MatkDown { range, durExt, potencies }
  , PdefDown { range, durExt, potencies }
  , MdefDown { range, durExt, potencies }
  , FireResistDown { range, durExt, potencies }
  , IceResistDown { range, durExt, potencies }
  , ThunderResistDown { range, durExt, potencies }
  , EarthResistDown { range, durExt, potencies }
  , WaterResistDown { range, durExt, potencies }
  , WindResistDown { range, durExt, potencies }
  , FireWeakness { range, durExt, percentage }
  , IceWeakness { range, durExt, percentage }
  , ThunderWeakness { range, durExt, percentage }
  , EarthWeakness { range, durExt, percentage }
  , WaterWeakness { range, durExt, percentage }
  , WindWeakness { range, durExt, percentage }
  , Enfeeble { range, durExt }
  , Stop { range, durExt }
  , ExploitWeakness { range, durExt, percentage }
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
    ThunderDamageUp _ -> unit
    EarthDamageUp _ -> unit
    WaterDamageUp _ -> unit
    WindDamageUp _ -> unit
    Veil _ -> unit
    Provoke _ -> unit
    PatkDown _ -> unit
    MatkDown _ -> unit
    PdefDown _ -> unit
    MdefDown _ -> unit
    FireResistDown _ -> unit
    IceResistDown _ -> unit
    ThunderResistDown _ -> unit
    EarthResistDown _ -> unit
    WaterResistDown _ -> unit
    WindResistDown _ -> unit
    FireWeakness _ -> unit
    IceWeakness _ -> unit
    ThunderWeakness _ -> unit
    EarthWeakness _ -> unit
    WaterWeakness _ -> unit
    WindWeakness _ -> unit
    Enfeeble _ -> unit
    Stop _ -> unit
    -- Unfortunately, there's no way to ensure our returns values are exhaustive
    -- https://alistairb.dev/exhaustive-return-warning/
    --
    -- As a workaround, we have this useless pattern match here to remind us to update the list above when new variants are added.
    ExploitWeakness _ -> unit

instance Display Potency where
  display =
    case _ of
      Low -> "Low"
      Mid -> "Mid"
      High -> "High"
      ExtraHigh -> "Extra High"
