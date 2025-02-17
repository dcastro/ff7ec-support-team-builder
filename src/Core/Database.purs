module Core.Database (init, createDb, writeToCache) where

import Core.Database.VLatest
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Core.Display (display)
import Core.Weapons.Parser as P
import Core.WebStorage as WS
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NAR
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), hush)
import Data.Foldable as F
import Data.List as List
import Data.List.Lazy as LazyList
import Data.List.ZipList (ZipList(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Time.Duration (Hours(..))
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Google.SheetsApi as SheetsApi
import Partial.Unsafe (unsafeCrashWith)
import Utils (MapAsArray(..), SetAsArray(..), logOnLeft, renderJsonErr, throwOnNothing, whenJust)
import Yoga.JSON as J

currentDbVersion :: Int
currentDbVersion = 1

init :: Aff (Maybe Db)
init = do
  runExceptT readFromCache >>= case _ of
    Left _ -> do
      Console.log "Db not found in cache, loading it from the spreadsheet..."
      dbMb <- hush <$> runExceptT (loadAndCreateDb Map.empty)
      whenJust dbMb $ writeToCache
      pure dbMb
    Right { db, hasExpired } | hasExpired -> do
      Console.log "Db found in cache but has expired, updating cache..."
      hush <$> runExceptT (loadAndCreateDb db.allWeapons) >>= case _ of
        Just updatedDb -> do
          writeToCache updatedDb
          pure $ Just updatedDb
        Nothing -> do
          Console.log "Failed to update db"
          pure $ Just db
    Right { db, hasExpired: _ } -> do
      Console.log "Db found in cache."
      pure $ Just db

  where
  -- Load the weapons from the spreadsheet, and updates the existing db.
  loadAndCreateDb :: forall f. MonadAff f => MonadThrow Unit f => MonadRec f => Map WeaponName WeaponData -> f Db
  loadAndCreateDb existingWeapons = do
    weapons <- loadFromSpreadsheet
    createDb weapons existingWeapons

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

getDistinctObs :: Weapon -> NonEmptyArray ObRange
getDistinctObs _ =
  -- TODO
  NAR.singleton (ObRange { from: FromOb6, to: Just ToOb10 })
    # NAR.cons (ObRange { from: FromOb0, to: Just ToOb5 })

pickOb :: ObRange -> NonEmptyArray ObRange -> ObRange
pickOb _ _ = do
  -- TODO
  ObRange { from: FromOb6, to: Just ToOb10 }

createDb :: forall m. MonadEffect m => MonadRec m => Array Weapon -> Map WeaponName WeaponData -> m Db
createDb newWeapons existingWeapons = do
  Arr.foldRecM
    (\db weapon -> insertWeapon weapon existingWeapons db)
    newDb
    newWeapons
  where
  newDb :: Db
  newDb =
    { allWeapons: Map.empty
    , groupedByEffect: Map.empty
    , allCharacterNames: Set.empty
    }

insertWeapon
  :: forall m
   . MonadEffect m
  => Weapon
  -> Map WeaponName WeaponData
  -> Db
  -> m Db
insertWeapon weapon existingWeapons db = do
  let groups = groupsForWeapon weapon
  if List.null groups then pure db
  else
    case Map.lookup weapon.name existingWeapons of
      Just existingWeapon -> do
        Console.log $ "Weapon already exists, replacing it: " <> display weapon.name
        pure $ db
          # mergeWithExisting existingWeapon
          # insertIntoGroups groups
          # insertCharacterName
      Nothing -> do
        Console.log $ "Weapon added: " <> display weapon.name
        pure $ db
          # insert
          # insertIntoGroups groups
          # insertCharacterName
  where

  insert :: Db -> Db
  insert db = do
    let distinctObs = getDistinctObs weapon
    let
      newWeapon =
        { weapon
        , ignored: false
        , distinctObs
        , ownedOb: Just $ NAR.last distinctObs
        }
    db { allWeapons = Map.insert weapon.name newWeapon db.allWeapons }

  mergeWithExisting :: WeaponData -> Db -> Db
  mergeWithExisting existing db = do
    let distinctObs = getDistinctObs weapon
    let
      newWeapon =
        { weapon
        , ignored: existing.ignored
        , distinctObs
        , ownedOb: existing.ownedOb <#> \owned -> pickOb owned distinctObs
        }
    db { allWeapons = Map.insert weapon.name newWeapon db.allWeapons }

  insertIntoGroups :: List.List GroupEntry -> Db -> Db
  insertIntoGroups groups db =
    F.foldr
      insertIntoGroup
      db
      groups

  insertIntoGroup :: GroupEntry -> Db -> Db
  insertIntoGroup { effectType, groupedWeapon } db = do
    let
      groupedByEffect = Map.alter
        ( case _ of
            Just weapons -> Just $ Arr.snoc weapons groupedWeapon
            Nothing -> Just [ groupedWeapon ]
        )
        effectType
        db.groupedByEffect
    db { groupedByEffect = groupedByEffect }

  insertCharacterName :: Db -> Db
  insertCharacterName db =
    db { allCharacterNames = Set.insert weapon.character db.allCharacterNames }

type GroupEntry =
  { effectType :: FilterEffectType
  , groupedWeapon :: GroupedWeapon
  }

groupsForWeapon :: Weapon -> List.List GroupEntry
groupsForWeapon weapon = do
  let entries = LazyList.catMaybes $ unwrap groupsForWeapon'

  let
    entries' =
      if weapon.cureAllAbility then
        entries
          # LazyList.cons
              { effectType: FilterHeal
              , groupedWeapon:
                  { weaponName: weapon.name
                  , ranges:
                      [ { range: All
                        , allPotencies: Nothing
                        }
                      ]
                  }
              }
      else entries

  mergeRanges entries'

  where
  groupsForWeapon' :: ZipList (Maybe GroupEntry)
  groupsForWeapon' = ado
    -- INVARIANT: this assumes weapon effects are listed in the same order at all overboost levels.
    ob0 <- ZipList $ LazyList.fromFoldable weapon.ob0.effects
    ob1 <- ZipList $ LazyList.fromFoldable weapon.ob1.effects
    ob6 <- ZipList $ LazyList.fromFoldable weapon.ob6.effects
    ob10 <- ZipList $ LazyList.fromFoldable weapon.ob10.effects
    in
      groupForWeaponEffect ob0 ob1 ob6 ob10 <#> \{ effectType, potencies } ->
        { effectType
        , groupedWeapon:
            { weaponName: weapon.name
            -- INVARIANT: this assumes an effect has the same range at all overboost levels.
            , ranges:
                [ { range: ob0.range
                  , allPotencies: potencies
                  }
                ]
            }
        }

  -- If there are many ranges for the same effect (e.g. Arctic Star has PATK Up SingleTarget & PATK Up Self),
  -- this function will merge those `GroupEntry`s into a single one.
  mergeRanges :: LazyList.List GroupEntry -> List.List GroupEntry
  mergeRanges groupEntries = do
    let
      (merged :: Map FilterEffectType GroupEntry) =
        LazyList.foldl
          ( \map ge ->
              Map.alter
                ( case _ of
                    Nothing -> Just ge
                    Just existingGe ->
                      Just $ existingGe
                        { groupedWeapon
                            { ranges =
                                existingGe.groupedWeapon.ranges
                                  <>
                                    ge.groupedWeapon.ranges
                            }
                        }
                )
                ge.effectType
                map
          )
          Map.empty
          groupEntries
    Map.values merged

  groupForWeaponEffect
    :: WeaponEffect
    -> WeaponEffect
    -> WeaponEffect
    -> WeaponEffect
    -> Maybe
         { effectType :: FilterEffectType
         , potencies :: Maybe AllPotencies
         }
  groupForWeaponEffect ob0 ob1 ob6 ob10 = do
    case ob0.effectType of
      Heal { percentage } ->
        if unwrap percentage >= 35 then Just { effectType: FilterHeal, potencies: Nothing }
        else Nothing
      Veil _ -> Just { effectType: FilterVeil, potencies: Nothing }
      Provoke _ -> Just { effectType: FilterProvoke, potencies: Nothing }
      Enfeeble _ -> Just { effectType: FilterEnfeeble, potencies: Nothing }
      Stop _ -> Just { effectType: FilterStop, potencies: Nothing }
      ExploitWeakness _ -> Just { effectType: FilterExploitWeakness, potencies: Nothing }
      PatkUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        PatkUp ob1, PatkUp ob6, PatkUp ob10 -> Just { effectType: FilterPatkUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      MatkUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        MatkUp ob1, MatkUp ob6, MatkUp ob10 -> Just { effectType: FilterMatkUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      PdefUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        PdefUp ob1, PdefUp ob6, PdefUp ob10 -> Just { effectType: FilterPdefUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      MdefUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        MdefUp ob1, MdefUp ob6, MdefUp ob10 -> Just { effectType: FilterMdefUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      FireDamageUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        FireDamageUp ob1, FireDamageUp ob6, FireDamageUp ob10 -> Just { effectType: FilterFireDamageUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      IceDamageUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        IceDamageUp ob1, IceDamageUp ob6, IceDamageUp ob10 -> Just { effectType: FilterIceDamageUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      ThunderDamageUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        ThunderDamageUp ob1, ThunderDamageUp ob6, ThunderDamageUp ob10 -> Just { effectType: FilterThunderDamageUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      EarthDamageUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        EarthDamageUp ob1, EarthDamageUp ob6, EarthDamageUp ob10 -> Just { effectType: FilterEarthDamageUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      WaterDamageUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        WaterDamageUp ob1, WaterDamageUp ob6, WaterDamageUp ob10 -> Just { effectType: FilterWaterDamageUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      WindDamageUp { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        WindDamageUp ob1, WindDamageUp ob6, WindDamageUp ob10 -> Just { effectType: FilterWindDamageUp, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      PatkDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        PatkDown ob1, PatkDown ob6, PatkDown ob10 -> Just { effectType: FilterPatkDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      MatkDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        MatkDown ob1, MatkDown ob6, MatkDown ob10 -> Just { effectType: FilterMatkDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      PdefDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        PdefDown ob1, PdefDown ob6, PdefDown ob10 -> Just { effectType: FilterPdefDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      MdefDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        MdefDown ob1, MdefDown ob6, MdefDown ob10 -> Just { effectType: FilterMdefDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      FireResistDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        FireResistDown ob1, FireResistDown ob6, FireResistDown ob10 -> Just { effectType: FilterFireResistDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      IceResistDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        IceResistDown ob1, IceResistDown ob6, IceResistDown ob10 -> Just { effectType: FilterIceResistDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      ThunderResistDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        ThunderResistDown ob1, ThunderResistDown ob6, ThunderResistDown ob10 -> Just { effectType: FilterThunderResistDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      EarthResistDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        EarthResistDown ob1, EarthResistDown ob6, EarthResistDown ob10 -> Just { effectType: FilterEarthResistDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      WaterResistDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        WaterResistDown ob1, WaterResistDown ob6, WaterResistDown ob10 -> Just { effectType: FilterWaterResistDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      WindResistDown { potencies: ob0Potencies } -> case ob1.effectType, ob6.effectType, ob10.effectType of
        WindResistDown ob1, WindResistDown ob6, WindResistDown ob10 -> Just { effectType: FilterWindResistDown, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit

  crash _ = unsafeCrashWith $ "Effects for weapon " <> display weapon.name <> " are not in the same order"

-- Throws if the cache is empty OR the cache data is corrupted.
readFromCache :: forall m. MonadThrow Unit m => MonadAff m => m { db :: Db, hasExpired :: Boolean }
readFromCache = do
  dbStr <- throwOnNothing $ WS.getItem "db"
  lastUpdatedStr <- throwOnNothing $ WS.getItem "last_updated"

  db :: Db <- fromSerializable <$> J.readJSON dbStr `logOnLeft` \err ->
    "Failed to deserialize db:\n" <> renderJsonErr err
  lastUpdated :: DateTime <- J.readJSON lastUpdatedStr `logOnLeft` \err ->
    "Failed to deserialize db:\n" <> renderJsonErr err

  now <- liftEffect Now.nowDateTime
  let hasExpired = DateTime.diff now lastUpdated > Hours 24.0

  pure { db, hasExpired }
  where
  fromSerializable :: SerializableDb -> Db
  fromSerializable db =
    { allWeapons: unwrap db.allWeapons
    , groupedByEffect: unwrap db.groupedByEffect
    , allCharacterNames: unwrap db.allCharacterNames
    }

writeToCache :: forall m. MonadAff m => Db -> m Unit
writeToCache db = do
  let dbStr = J.writeJSON $ toSerializable db
  lastUpdatedStr <- J.writeJSON <$> liftEffect Now.nowDateTime
  let currentDbVersionStr = J.writeJSON currentDbVersion

  WS.setItem "db" dbStr
  WS.setItem "last_updated" lastUpdatedStr
  WS.setItem "db_version" currentDbVersionStr
  where
  toSerializable :: Db -> SerializableDb
  toSerializable db =
    { allWeapons: MapAsArray db.allWeapons
    , groupedByEffect: MapAsArray db.groupedByEffect
    , allCharacterNames: SetAsArray db.allCharacterNames
    }
