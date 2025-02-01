module Core.Armory where

import Core.Database.VLatest
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Core.Display (display)
import Core.Weapons.Parser (Weapon)
import Core.Weapons.Parser as P
import Core.WebStorage as WS
import Data.Array as Arr
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Foldable as F
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty as NES
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
import Utils (MapAsArray(..), SetAsArray(..), logOnLeft, renderJsonErr, throwOnNothing, whenJust)
import Yoga.JSON as J

currentDbVersion :: Int
currentDbVersion = 1

type GroupEntry =
  { filter :: Filter
  , weaponName :: WeaponName
  , potenciesAtOb10 :: Maybe Potencies
  }

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
  , allCharacterNames: Set.empty
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
      Console.log $ "Weapon already exists, replacing it: " <> display weapon.name
      pure $ armory
        # mergeWithExisting existingWeapon
        # insertIntoGroups
        # insertCharacterName
    Nothing -> do
      Console.log $ "Weapon added: " <> display weapon.name
      pure $ armory
        # insert
        # insertIntoGroups
        # insertCharacterName
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

    Veil _ -> Just { effectType: FilterVeil, potencies: Nothing }
    Provoke _ -> Just { effectType: FilterProvoke, potencies: Nothing }
    Enfeeble _ -> Just { effectType: FilterEnfeeble, potencies: Nothing }
    Stop _ -> Just { effectType: FilterStop, potencies: Nothing }
    ExploitWeakness _ -> Just { effectType: FilterExploitWeakness, potencies: Nothing }

    PatkUp { potencies } -> Just { effectType: FilterPatkUp, potencies: Just potencies }
    MatkUp { potencies } -> Just { effectType: FilterMatkUp, potencies: Just potencies }
    PdefUp { potencies } -> Just { effectType: FilterPdefUp, potencies: Just potencies }
    MdefUp { potencies } -> Just { effectType: FilterMdefUp, potencies: Just potencies }
    FireDamageUp { potencies } -> Just { effectType: FilterFireDamageUp, potencies: Just potencies }
    IceDamageUp { potencies } -> Just { effectType: FilterIceDamageUp, potencies: Just potencies }
    ThunderDamageUp { potencies } -> Just { effectType: FilterThunderDamageUp, potencies: Just potencies }
    EarthDamageUp { potencies } -> Just { effectType: FilterEarthDamageUp, potencies: Just potencies }
    WaterDamageUp { potencies } -> Just { effectType: FilterWaterDamageUp, potencies: Just potencies }
    WindDamageUp { potencies } -> Just { effectType: FilterWindDamageUp, potencies: Just potencies }

    PatkDown { potencies } -> Just { effectType: FilterPatkDown, potencies: Just potencies }
    MatkDown { potencies } -> Just { effectType: FilterMatkDown, potencies: Just potencies }
    PdefDown { potencies } -> Just { effectType: FilterPdefDown, potencies: Just potencies }
    MdefDown { potencies } -> Just { effectType: FilterMdefDown, potencies: Just potencies }
    FireResistDown { potencies } -> Just { effectType: FilterFireResistDown, potencies: Just potencies }
    IceResistDown { potencies } -> Just { effectType: FilterIceResistDown, potencies: Just potencies }
    ThunderResistDown { potencies } -> Just { effectType: FilterThunderResistDown, potencies: Just potencies }
    EarthResistDown { potencies } -> Just { effectType: FilterEarthResistDown, potencies: Just potencies }
    WaterResistDown { potencies } -> Just { effectType: FilterWaterResistDown, potencies: Just potencies }
    WindResistDown { potencies } -> Just { effectType: FilterWindResistDown, potencies: Just potencies }

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

    if weapon.cureAllAbility then do
      -- Assuming a 5* Cura materia (100% heal) with a -40% penalty
      let healAll = { effectType: Heal { percentage: Percentage 60 }, range: All }
      Set.union groupEntries (Set.fromFoldable $ matchingFilters' healAll)

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

  insertCharacterName :: Armory -> Armory
  insertCharacterName armory =
    armory { allCharacterNames = Set.insert weapon.character armory.allCharacterNames }

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
    , allCharacterNames: unwrap armory.allCharacterNames
    }

writeToCache :: forall m. MonadAff m => Armory -> m Unit
writeToCache armory = do
  let armoryStr = J.writeJSON $ toSerializable armory
  lastUpdatedStr <- J.writeJSON <$> liftEffect Now.nowDateTime
  let currentDbVersionStr = J.writeJSON currentDbVersion

  WS.setItem "armory" armoryStr
  WS.setItem "last_updated" lastUpdatedStr
  WS.setItem "db_version" currentDbVersionStr
  where
  toSerializable :: Armory -> SerializableArmory
  toSerializable armory =
    { allWeapons: MapAsArray armory.allWeapons
    , groupedByEffect: MapAsArray armory.groupedByEffect
    , allCharacterNames: SetAsArray armory.allCharacterNames
    }
