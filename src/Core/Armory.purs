module Core.Armory where

import Core.Weapons.Types
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Core.Weapons.Parser as P
import Core.WebStorage as WS
import Data.Array as Arr
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Foldable as F
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Time.Duration (Hours(..))
import Data.Unfoldable as Unfoldable
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Google.SheetsApi as SheetsApi
import Record as Record
import Type.Proxy (Proxy(..))
import Utils (MapAsArray(..), logOnLeft, renderJsonErr, throwOnNothing, whenJust)
import Utils as Utils
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as J
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum

type Armory =
  { allWeapons :: Map WeaponName ArmoryWeapon
  , groupedByEffect :: Map Filter (Array GroupedWeapon)
  }

type SerializableArmory =
  { allWeapons :: MapAsArray WeaponName ArmoryWeapon
  , groupedByEffect :: MapAsArray Filter (Array GroupedWeapon)
  }

type GroupedWeapon =
  { weaponName :: WeaponName
  , potenciesAtOb10 :: Maybe Potencies
  }

type GroupEntry =
  { filter :: Filter
  , weaponName :: WeaponName
  , potenciesAtOb10 :: Maybe Potencies
  }

type ArmoryWeapon =
  { name :: WeaponName
  , character :: CharacterName
  , source :: NonEmptyString
  , image :: NonEmptyString
  , ob0 :: ObLevel
  , ob1 :: ObLevel
  , ob6 :: ObLevel
  , ob10 :: ObLevel
  , cureAllAbility :: Boolean
  , ignored :: Boolean
  }

type Filter =
  { effectType :: FilterEffectType
  , range :: FilterRange
  }

data FilterRange
  = FilterAll
  | FilterSingleTargetOrAll
  | FilterSelfOrSingleTargetOrAll

derive instance Generic FilterRange _
derive instance Eq FilterRange
derive instance Ord FilterRange
instance Show FilterRange where
  show = genericShow

instance WriteForeign FilterRange where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance ReadForeign FilterRange where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

allFilterRanges :: Array FilterRange
allFilterRanges = Utils.listEnum

data FilterEffectType
  = FilterHeal
  -- Buffs
  | FilterVeil
  | FilterProvoke
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
  -- Debuffs
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

derive instance Generic FilterEffectType _
derive instance Eq FilterEffectType
derive instance Ord FilterEffectType
instance Show FilterEffectType where
  show = genericShow

instance WriteForeign FilterEffectType where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance ReadForeign FilterEffectType where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

allFilterEffectTypes :: Array FilterEffectType
allFilterEffectTypes = Utils.listEnum

init :: Aff (Maybe Armory)
init = do
  runExceptT readFromCache >>= case _ of
    Left _ -> do
      Console.log "Armory not found in cache, loading it from the spreadsheet..."
      armoryMb <- hush <$> runExceptT (loadAndCreateArmory Map.empty)
      whenJust armoryMb $ writeToCache
      pure armoryMb
    Right { armory, hasExpired } | hasExpired -> do
      Console.log "Armory found in cache but has expired, updating cache..."
      hush <$> runExceptT (loadAndCreateArmory armory.allWeapons) >>= case _ of
        Just updatedArmory -> do
          writeToCache updatedArmory
          pure $ Just updatedArmory
        Nothing -> do
          Console.log "Failed to updated armory"
          pure $ Just armory
    Right { armory, hasExpired: _ } -> do
      Console.log "Armory found in cache."
      pure $ Just armory
  where
  -- Load the weapons from the spreadsheet, and updating the existing armory.
  loadAndCreateArmory :: forall f. MonadAff f => MonadThrow Unit f => MonadRec f => Map WeaponName ArmoryWeapon -> f Armory
  loadAndCreateArmory existingWeapons = do
    weapons <- loadFromSpreadsheet
    createArmory weapons existingWeapons

  -- Throws if we can't parse the Google Sheet.
  loadFromSpreadsheet :: forall f. MonadAff f => MonadThrow Unit f => f (Array Weapon)
  loadFromSpreadsheet = do
    table <- liftAff $ SheetsApi.getSheet "Weapons!A:Z"

    let { weapons, errors } = P.parseWeapons table.result.values
    for_ errors \err -> Console.log $ "Failed to parse weapon:\n" <> err

    when (Arr.null weapons) do
      Console.log "Failed to parse any weapons"
      throwError unit
    pure weapons

newArmory :: Armory
newArmory =
  { allWeapons: Map.empty
  , groupedByEffect: Map.empty
  }

createArmory :: forall m. MonadEffect m => MonadRec m => Array Weapon -> Map WeaponName ArmoryWeapon -> m Armory
createArmory newWeapons existingWeapons = do
  Arr.foldRecM
    (\armory weapon -> insertWeapon weapon existingWeapons armory)
    newArmory
    newWeapons

insertWeapon
  :: forall m
   . MonadEffect m
  => Weapon
  -> Map WeaponName ArmoryWeapon
  -> Armory
  -> m Armory
insertWeapon weapon existingWeapons armory =
  case Map.lookup weapon.name existingWeapons of
    Just existingWeapon -> do
      Console.log $ "Weapon already exists, replacing it: " <> show weapon.name
      pure $ armory
        # mergeWithExisting existingWeapon
        # insertIntoGroups
    Nothing -> do
      Console.log $ "Weapon added: " <> show weapon.name
      pure $ armory
        # insert
        # insertIntoGroups
  where

  insert :: Armory -> Armory
  insert armory = do
    -- By default, we ignore only "Event" weapons.
    let ignored = if NES.toString weapon.source == "Event" then true else false
    let armoryWeapon = Record.insert (Proxy :: Proxy "ignored") ignored weapon
    armory { allWeapons = Map.insert armoryWeapon.name armoryWeapon armory.allWeapons }

  mergeWithExisting :: ArmoryWeapon -> Armory -> Armory
  mergeWithExisting existing armory = do
    let armoryWeapon = Record.insert (Proxy :: Proxy "ignored") existing.ignored weapon
    armory { allWeapons = Map.insert armoryWeapon.name armoryWeapon armory.allWeapons }

  insertIntoGroups :: Armory -> Armory
  insertIntoGroups armory =
    F.foldr
      addToGroup
      armory
      matchingFilters

  matchingEffectType :: EffectType -> Maybe { effectType :: FilterEffectType, potencies :: Maybe Potencies }
  matchingEffectType = case _ of
    Heal { percentage } ->
      if unwrap percentage >= 35 then Just { effectType: FilterHeal, potencies: Nothing }
      else Nothing
    -- Buffs
    Veil -> Just { effectType: FilterVeil, potencies: Nothing }
    Provoke -> Just { effectType: FilterProvoke, potencies: Nothing }
    PatkUp potencies -> Just { effectType: FilterPatkUp, potencies: Just potencies }
    MatkUp potencies -> Just { effectType: FilterMatkUp, potencies: Just potencies }
    PdefUp potencies -> Just { effectType: FilterPdefUp, potencies: Just potencies }
    MdefUp potencies -> Just { effectType: FilterMdefUp, potencies: Just potencies }
    FireDamageUp potencies -> Just { effectType: FilterFireDamageUp, potencies: Just potencies }
    IceDamageUp potencies -> Just { effectType: FilterIceDamageUp, potencies: Just potencies }
    ThunderDamageUp potencies -> Just { effectType: FilterThunderDamageUp, potencies: Just potencies }
    EarthDamageUp potencies -> Just { effectType: FilterEarthDamageUp, potencies: Just potencies }
    WaterDamageUp potencies -> Just { effectType: FilterWaterDamageUp, potencies: Just potencies }
    WindDamageUp potencies -> Just { effectType: FilterWindDamageUp, potencies: Just potencies }
    -- Debuffs
    PatkDown potencies -> Just { effectType: FilterPatkDown, potencies: Just potencies }
    MatkDown potencies -> Just { effectType: FilterMatkDown, potencies: Just potencies }
    PdefDown potencies -> Just { effectType: FilterPdefDown, potencies: Just potencies }
    MdefDown potencies -> Just { effectType: FilterMdefDown, potencies: Just potencies }
    FireResistDown potencies -> Just { effectType: FilterFireResistDown, potencies: Just potencies }
    IceResistDown potencies -> Just { effectType: FilterIceResistDown, potencies: Just potencies }
    ThunderResistDown potencies -> Just { effectType: FilterThunderResistDown, potencies: Just potencies }
    EarthResistDown potencies -> Just { effectType: FilterEarthResistDown, potencies: Just potencies }
    WaterResistDown potencies -> Just { effectType: FilterWaterResistDown, potencies: Just potencies }
    WindResistDown potencies -> Just { effectType: FilterWindResistDown, potencies: Just potencies }

  matchingRanges :: Range -> Array FilterRange
  matchingRanges = case _ of
    Self -> [ FilterSelfOrSingleTargetOrAll ]
    SingleTarget -> [ FilterSelfOrSingleTargetOrAll, FilterSingleTargetOrAll ]
    All -> [ FilterSelfOrSingleTargetOrAll, FilterSingleTargetOrAll, FilterAll ]

  matchingFilters' :: WeaponEffect -> Array GroupEntry
  matchingFilters' { effectType, range } = do
    range <- matchingRanges range
    { effectType, potencies } <- Unfoldable.fromMaybe $ matchingEffectType effectType
    pure
      { filter: { effectType, range }
      , weaponName: weapon.name
      , potenciesAtOb10: potencies
      }

  matchingFilters :: Set GroupEntry
  matchingFilters = do
    let groupEntries = Set.fromFoldable $ weapon.ob10.effects >>= \effect -> matchingFilters' effect

    if weapon.cureAllAbility then
      Set.insert
        { filter: { effectType: FilterHeal, range: FilterAll }
        , weaponName: weapon.name
        , potenciesAtOb10: Nothing
        }
        groupEntries
    else groupEntries

  addToGroup :: GroupEntry -> Armory -> Armory
  addToGroup { filter, weaponName, potenciesAtOb10 } armory = do
    let
      groupedWeapon = { weaponName, potenciesAtOb10 } :: GroupedWeapon
      groupedByEffect = Map.alter
        ( case _ of
            Just weapons -> Just $ Arr.snoc weapons groupedWeapon
            Nothing -> Just [ groupedWeapon ]
        )
        filter
        armory.groupedByEffect
    armory { groupedByEffect = groupedByEffect }

-- Throws if the cache is empty OR the cache data is corrupted.
readFromCache :: forall m. MonadThrow Unit m => MonadAff m => m { armory :: Armory, hasExpired :: Boolean }
readFromCache = do
  armoryStr <- throwOnNothing $ WS.getItem "armory"
  lastUpdatedStr <- throwOnNothing $ WS.getItem "last_updated"

  armory :: Armory <- fromSerializable <$> J.readJSON armoryStr `logOnLeft` \err ->
    "Failed to deserialize armory:\n" <> renderJsonErr err
  lastUpdated :: DateTime <- J.readJSON lastUpdatedStr `logOnLeft` \err ->
    "Failed to deserialize armory:\n" <> renderJsonErr err

  now <- liftEffect Now.nowDateTime
  let hasExpired = DateTime.diff now lastUpdated > Hours 24.0

  pure { armory, hasExpired }
  where
  fromSerializable :: SerializableArmory -> Armory
  fromSerializable armory =
    { allWeapons: unwrap armory.allWeapons
    , groupedByEffect: unwrap armory.groupedByEffect
    }

writeToCache :: forall m. MonadAff m => Armory -> m Unit
writeToCache armory = do
  let armoryStr = J.writeJSON $ toSerializable armory
  lastUpdatedStr <- J.writeJSON <$> liftEffect Now.nowDateTime

  WS.setItem "armory" armoryStr
  WS.setItem "last_updated" lastUpdatedStr
  where
  toSerializable :: Armory -> SerializableArmory
  toSerializable armory =
    { allWeapons: MapAsArray armory.allWeapons
    , groupedByEffect: MapAsArray armory.groupedByEffect
    }
