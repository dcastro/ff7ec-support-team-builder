module Core.Database
  ( init
  , createDbState
  , writeToCache

  -- Exported for tests
  , parseAndMigrateUserState
  , toSerializableUserState
  ) where

import Core.Database.Types
import Core.Database.UserState.VLatest
import Prelude

import Control.Alt (alt)
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Core.Database.UserState.V1 as V1
import Core.Database.UserState.VLatest as V2
import Core.Database.UserState.VLatest as VLatest
import Core.Display (display)
import Core.Weapons.Parser as P
import Core.Weapons.Parser as Parser
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
import Data.String.NonEmpty as NES
import Data.Time.Duration (Hours(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Google.SheetsApi as SheetsApi
import Parsing (runParser)
import Partial.Unsafe (unsafeCrashWith)
import Utils (MapAsArray(..), SetAsArray(..), throwOnLeft, renderJsonErr, the, throwOnNothing, unsafeFromJust, whenJust)
import Yoga.JSON as J

currentUserStateVersion :: Int
currentUserStateVersion = 2

init :: Aff (Maybe DbState)
init = do
  runExceptT readFromCache >>= case _ of
    Left (err :: String) -> do
      Console.error err
      Console.log "Loading db from the spreadsheet..."
      dbMb <- hush <$> runExceptT (loadAndCreateDbState newUserState)
      whenJust dbMb $ writeToCache
      pure dbMb
    Right { userState, dbMaybe: Just { db, hasExpired } } | hasExpired -> do
      Console.log "Db found in cache but has expired, updating cache..."
      hush <$> runExceptT (loadAndCreateDbState userState) >>= case _ of
        Just updatedDbState -> do
          writeToCache updatedDbState
          pure $ Just updatedDbState
        Nothing -> do
          Console.error "Failed to update db, reusing existing expired db."
          pure $ Just { userState, db }
    Right { userState, dbMaybe: Nothing } -> do
      Console.log "Db not found in cache, updating cache..."
      hush <$> runExceptT (loadAndCreateDbState userState) >>= case _ of
        Just updatedDbState -> do
          writeToCache updatedDbState
          pure $ Just updatedDbState
        Nothing -> do
          Console.error "Failed to update db."
          pure $ Nothing
    Right { userState, dbMaybe: Just { db, hasExpired: _ } } -> do
      Console.log "Db found in cache."
      pure $ Just { userState, db }

  where
  -- Load the weapons from the spreadsheet, and updates the existing db.
  loadAndCreateDbState :: forall f. MonadAff f => MonadThrow Unit f => MonadRec f => UserState -> f DbState
  loadAndCreateDbState existingUserState = do
    weapons <- loadFromSpreadsheet
    createDbState weapons existingUserState

  -- Throws if we can't parse the Google Sheet.
  loadFromSpreadsheet :: forall f. MonadAff f => MonadThrow Unit f => f (Array Weapon)
  loadFromSpreadsheet = do
    table <- liftAff $ SheetsApi.getSheet "Weapons!A:ZZ"

    let { weapons, errors } = P.parseWeapons table.result.values
    for_ errors \err -> Console.log $ "Failed to parse weapon:\n" <> err

    when (Arr.null weapons) do
      Console.log "Failed to parse any weapons"
      throwError unit
    pure weapons

newUserState :: UserState
newUserState = { weapons: Map.empty }

getDistinctObs :: Weapon -> NonEmptyArray ObRange
getDistinctObs weapon = do
  NAR.cons' (weapon.ob0 /\ FromOb0 /\ ToOb0)
    [ weapon.ob1 /\ FromOb1 /\ ToOb5
    , weapon.ob6 /\ FromOb6 /\ ToOb9
    , weapon.ob10 /\ FromOb10 /\ ToOb10
    ]
    # NAR.groupBy (\(Tuple x _) (Tuple y _) -> areObLevelsEquivalent x y)
    <#> \(group :: NonEmptyArray (ObLevel /\ FromOb /\ ToOb)) ->
      ObRange
        { from: NAR.head group # \(Tuple _ (Tuple from _)) -> from
        , to: NAR.last group # \(Tuple _ (Tuple _ to)) -> to
        }
  where
  areObLevelsEquivalent :: ObLevel -> ObLevel -> Boolean
  areObLevelsEquivalent obx oby = do
    indices (Arr.length obx.effects)
      # Arr.all \idx -> do
          let effect1 = Arr.index obx.effects idx `unsafeFromJust` ("Index out of bounds for weapon: " <> display weapon.name)
          let effect2 = Arr.index oby.effects idx `unsafeFromJust` ("Index out of bounds for weapon: " <> display weapon.name)
          areWeaponEffectsEquivalent effect1 effect2

  indices :: Int -> Array Int
  indices n = if n <= 0 then [] else Arr.range 0 (n - 1)

  -- Two effects are equivalent if they have the same potencies and range
  -- Duration, extension, and percentages are not considered.
  areWeaponEffectsEquivalent :: WeaponEffect -> WeaponEffect -> Boolean
  areWeaponEffectsEquivalent x y =
    case x of
      Heal { range: range1, percentage: _ } ->
        case y of
          Heal { range: range2, percentage: _ } -> range1 == range2
          _ -> crash unit
      PatkUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          PatkUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      MatkUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          MatkUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      PdefUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          PdefUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      MdefUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          MdefUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      FireDamageUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          FireDamageUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      IceDamageUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          IceDamageUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      LightningDamageUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          LightningDamageUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      EarthDamageUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          EarthDamageUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      WaterDamageUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          WaterDamageUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      WindDamageUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          WindDamageUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      Veil { range: range1, durExt: _, percentage: _ } ->
        case y of
          Veil { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit
      Provoke { range: range1, durExt: _ } ->
        case y of
          Provoke { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      PatkDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          PatkDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      MatkDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          MatkDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      PdefDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          PdefDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      MdefDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          MdefDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      FireResistDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          FireResistDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      IceResistDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          IceResistDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      LightningResistDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          LightningResistDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      EarthResistDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          EarthResistDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      WaterResistDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          WaterResistDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      WindResistDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          WindResistDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      Enfeeble { range: range1, durExt: _ } ->
        case y of
          Enfeeble { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      Stop { range: range1, durExt: _ } ->
        case y of
          Stop { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      ExploitWeakness { range: range1, durExt: _, percentage: _ } ->
        case y of
          ExploitWeakness { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit
      HPGain { range: range1, durExt: _, percentage: _ } ->
        case y of
          HPGain { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit
      EnhanceBuffs { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          EnhanceBuffs { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      EnhanceDebuffs { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          EnhanceDebuffs { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      Enliven { range: range1, durExt: _ } ->
        case y of
          Enliven { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit

      FireWeakness { range: range1, durExt: _ } ->
        case y of
          FireWeakness { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      IceWeakness { range: range1, durExt: _ } ->
        case y of
          IceWeakness { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      LightningWeakness { range: range1, durExt: _ } ->
        case y of
          LightningWeakness { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      EarthWeakness { range: range1, durExt: _ } ->
        case y of
          EarthWeakness { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      WaterWeakness { range: range1, durExt: _ } ->
        case y of
          WaterWeakness { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      WindWeakness { range: range1, durExt: _ } ->
        case y of
          WindWeakness { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      PhysicalWeaponBoost { range: range1, durExt: _ } ->
        case y of
          PhysicalWeaponBoost { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      MagicWeaponBoost { range: range1, durExt: _ } ->
        case y of
          MagicWeaponBoost { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      PhysicalDamageBonus { range: range1, durExt: _ } ->
        case y of
          PhysicalDamageBonus { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      MagicDamageBonus { range: range1, durExt: _ } ->
        case y of
          MagicDamageBonus { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      FireWeaponBoost { range: range1, durExt: _ } ->
        case y of
          FireWeaponBoost { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      IceWeaponBoost { range: range1, durExt: _ } ->
        case y of
          IceWeaponBoost { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      LightningWeaponBoost { range: range1, durExt: _ } ->
        case y of
          LightningWeaponBoost { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      EarthWeaponBoost { range: range1, durExt: _ } ->
        case y of
          EarthWeaponBoost { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      WaterWeaponBoost { range: range1, durExt: _ } ->
        case y of
          WaterWeaponBoost { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      WindWeaponBoost { range: range1, durExt: _ } ->
        case y of
          WindWeaponBoost { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      FireDamageBonus { range: range1, durExt: _ } ->
        case y of
          FireDamageBonus { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      IceDamageBonus { range: range1, durExt: _ } ->
        case y of
          IceDamageBonus { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      LightningDamageBonus { range: range1, durExt: _ } ->
        case y of
          LightningDamageBonus { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      EarthDamageBonus { range: range1, durExt: _ } ->
        case y of
          EarthDamageBonus { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      WaterDamageBonus { range: range1, durExt: _ } ->
        case y of
          WaterDamageBonus { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      WindDamageBonus { range: range1, durExt: _ } ->
        case y of
          WindDamageBonus { range: range2, durExt: _ } -> range1 == range2
          _ -> crash unit
      FireDamageDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          FireDamageDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      IceDamageDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          IceDamageDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      LightningDamageDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          LightningDamageDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      EarthDamageDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          EarthDamageDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      WaterDamageDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          WaterDamageDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      WindDamageDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          WindDamageDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      FireResistUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          FireResistUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      IceResistUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          IceResistUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      LightningResistUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          LightningResistUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      EarthResistUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          EarthResistUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      WaterResistUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          WaterResistUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit
      WindResistUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          WindResistUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
          _ -> crash unit

  crash _ = unsafeCrashWith $ "Effects for weapon " <> display weapon.name <> " are not in the same order"

createDbState :: forall m. MonadEffect m => MonadRec m => Array Weapon -> UserState -> m DbState
createDbState newWeapons existingUserState = do
  (finalDb :: Db) <- Arr.foldRecM
    (\db weapon -> insertWeapon weapon db)
    newDb
    newWeapons

  let
    (finalUserState :: UserState) =
      F.foldl
        ( \userState weaponData ->
            case Map.lookup weaponData.weapon.name userState.weapons of
              -- If new weapons were added to the db, we need to create "empty states" for them.
              Nothing -> do
                let
                  updatedUserStateWeapons =
                    Map.insert weaponData.weapon.name
                      { ignored: false
                      , ownedOb: Just $ NAR.last weaponData.distinctObs
                      }
                      userState.weapons
                userState { weapons = updatedUserStateWeapons }
              Just existingWeaponState -> do
                -- @(ref:owned-ob-invariant)
                --
                -- Enforce the `UserStateWeapon.ownedOb` invariant: it must match one of
                -- the items in the corresponding `WeaponData.distinctObs`.
                -- If it doesn't, reset it.
                --
                -- This can happen when a new weapon with new effects is added to the sheet
                -- (and we set `distinctObs` to [OB0-10] and `ownedOb` to OB0-10),
                -- and then later we add support for that new effect,
                -- which changes the `distinctObs` for that weapon to e.g. [OB0-5, OB6-10].
                -- In that scenario, we have to manually correct the `ownedOb` to OB6-10.
                let
                  ownedObIsValid =
                    case existingWeaponState.ownedOb of
                      Just ownedOb -> NAR.elem ownedOb weaponData.distinctObs
                      Nothing -> true
                if ownedObIsValid then userState
                else do
                  let
                    updatedUserStateWeapons =
                      Map.insert weaponData.weapon.name
                        (existingWeaponState { ownedOb = Just $ NAR.last weaponData.distinctObs })
                        userState.weapons
                  userState { weapons = updatedUserStateWeapons }
        )
        existingUserState
        finalDb.allWeapons

  pure { db: finalDb, userState: finalUserState }

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
  -> Db
  -> m Db
insertWeapon weapon db = do
  let groups = groupsForWeapon weapon
  if List.null groups then pure db
  else
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
        , distinctObs
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

-- The range of an effect. Every `WeaponEffect` constructor carries a `range` field;
-- this extracts it without caring which constructor it is.
rangeOf :: WeaponEffect -> Range
rangeOf = case _ of
  Heal r -> r.range
  PatkUp r -> r.range
  MatkUp r -> r.range
  PdefUp r -> r.range
  MdefUp r -> r.range
  HPGain r -> r.range
  EnhanceBuffs r -> r.range
  PhysicalWeaponBoost r -> r.range
  MagicWeaponBoost r -> r.range
  PhysicalDamageBonus r -> r.range
  MagicDamageBonus r -> r.range
  FireDamageUp r -> r.range
  IceDamageUp r -> r.range
  LightningDamageUp r -> r.range
  EarthDamageUp r -> r.range
  WaterDamageUp r -> r.range
  WindDamageUp r -> r.range
  FireWeaponBoost r -> r.range
  IceWeaponBoost r -> r.range
  LightningWeaponBoost r -> r.range
  EarthWeaponBoost r -> r.range
  WaterWeaponBoost r -> r.range
  WindWeaponBoost r -> r.range
  FireDamageBonus r -> r.range
  IceDamageBonus r -> r.range
  LightningDamageBonus r -> r.range
  EarthDamageBonus r -> r.range
  WaterDamageBonus r -> r.range
  WindDamageBonus r -> r.range
  FireResistUp r -> r.range
  IceResistUp r -> r.range
  LightningResistUp r -> r.range
  EarthResistUp r -> r.range
  WaterResistUp r -> r.range
  WindResistUp r -> r.range
  Veil r -> r.range
  Provoke r -> r.range
  PatkDown r -> r.range
  MatkDown r -> r.range
  PdefDown r -> r.range
  MdefDown r -> r.range
  FireDamageDown r -> r.range
  IceDamageDown r -> r.range
  LightningDamageDown r -> r.range
  EarthDamageDown r -> r.range
  WaterDamageDown r -> r.range
  WindDamageDown r -> r.range
  FireResistDown r -> r.range
  IceResistDown r -> r.range
  LightningResistDown r -> r.range
  EarthResistDown r -> r.range
  WaterResistDown r -> r.range
  WindResistDown r -> r.range
  FireWeakness r -> r.range
  IceWeakness r -> r.range
  LightningWeakness r -> r.range
  EarthWeakness r -> r.range
  WaterWeakness r -> r.range
  WindWeakness r -> r.range
  Enfeeble r -> r.range
  Stop r -> r.range
  ExploitWeakness r -> r.range
  EnhanceDebuffs r -> r.range
  Enliven r -> r.range

-- The potencies of an effect, if it has any.
-- Some effects have no potencies (e.g. `Heal`, `Provoke`, `Enliven`,
-- and the percentage-based boosts), in which case this returns `Nothing`.
potenciesOf :: WeaponEffect -> Maybe Potencies
potenciesOf = case _ of
  Heal _ -> Nothing
  PatkUp r -> Just r.potencies
  MatkUp r -> Just r.potencies
  PdefUp r -> Just r.potencies
  MdefUp r -> Just r.potencies
  HPGain _ -> Nothing
  EnhanceBuffs r -> Just r.potencies
  PhysicalWeaponBoost _ -> Nothing
  MagicWeaponBoost _ -> Nothing
  PhysicalDamageBonus _ -> Nothing
  MagicDamageBonus _ -> Nothing
  FireDamageUp r -> Just r.potencies
  IceDamageUp r -> Just r.potencies
  LightningDamageUp r -> Just r.potencies
  EarthDamageUp r -> Just r.potencies
  WaterDamageUp r -> Just r.potencies
  WindDamageUp r -> Just r.potencies
  FireWeaponBoost _ -> Nothing
  IceWeaponBoost _ -> Nothing
  LightningWeaponBoost _ -> Nothing
  EarthWeaponBoost _ -> Nothing
  WaterWeaponBoost _ -> Nothing
  WindWeaponBoost _ -> Nothing
  FireDamageBonus _ -> Nothing
  IceDamageBonus _ -> Nothing
  LightningDamageBonus _ -> Nothing
  EarthDamageBonus _ -> Nothing
  WaterDamageBonus _ -> Nothing
  WindDamageBonus _ -> Nothing
  FireResistUp r -> Just r.potencies
  IceResistUp r -> Just r.potencies
  LightningResistUp r -> Just r.potencies
  EarthResistUp r -> Just r.potencies
  WaterResistUp r -> Just r.potencies
  WindResistUp r -> Just r.potencies
  Veil _ -> Nothing
  Provoke _ -> Nothing
  PatkDown r -> Just r.potencies
  MatkDown r -> Just r.potencies
  PdefDown r -> Just r.potencies
  MdefDown r -> Just r.potencies
  FireDamageDown r -> Just r.potencies
  IceDamageDown r -> Just r.potencies
  LightningDamageDown r -> Just r.potencies
  EarthDamageDown r -> Just r.potencies
  WaterDamageDown r -> Just r.potencies
  WindDamageDown r -> Just r.potencies
  FireResistDown r -> Just r.potencies
  IceResistDown r -> Just r.potencies
  LightningResistDown r -> Just r.potencies
  EarthResistDown r -> Just r.potencies
  WaterResistDown r -> Just r.potencies
  WindResistDown r -> Just r.potencies
  FireWeakness _ -> Nothing
  IceWeakness _ -> Nothing
  LightningWeakness _ -> Nothing
  EarthWeakness _ -> Nothing
  WaterWeakness _ -> Nothing
  WindWeakness _ -> Nothing
  Enfeeble _ -> Nothing
  Stop _ -> Nothing
  ExploitWeakness _ -> Nothing
  EnhanceDebuffs r -> Just r.potencies
  Enliven _ -> Nothing

groupsForWeapon :: Weapon -> List.List GroupEntry
groupsForWeapon weapon = do
  mergeRanges $ Arr.fold
    [ LazyList.catMaybes (unwrap groupsForWeapon')
    , getCureAllAbility
    , getCommandAbilityDiamondSigil
    , getSigilBoost
    ]
  where
  -- Check if the weapon has a Cure All S-Ability.
  -- #(ref:use-cure-spell)
  getCureAllAbility :: LazyList.List GroupEntry
  getCureAllAbility = do
    let sAbilities = [ weapon.sAbilities.slot1, weapon.sAbilities.slot2, weapon.sAbilities.slot3 ]
    if Arr.any Parser.hasCureAllSAbility sAbilities then
      LazyList.singleton
        { effectType: FilterHeal
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges: Just
                [ { allRanges: { ob0: All, ob1: All, ob6: All, ob10: All }
                  , allPotencies: Nothing
                  }
                ]
            }
        }
    else
      LazyList.nil

  -- Check if the weapon has a C. Ability Diamond Sigil.
  getCommandAbilityDiamondSigil :: LazyList.List GroupEntry
  getCommandAbilityDiamondSigil =
    case weapon.commandAbilitySigil of
      Just SigilDiamond -> LazyList.singleton
        { effectType: FilterSigilDiamond
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges: Nothing
            }
        }
      _ -> LazyList.nil

  -- Check if the weapon has a Sigil Boost S. Ability.
  -- #(ref:use-sigil-boosts)
  getSigilBoost :: LazyList.List GroupEntry
  getSigilBoost =
    LazyList.fromFoldable [ weapon.sAbilities.slot1, weapon.sAbilities.slot2, weapon.sAbilities.slot3 ]
      <#> (\sAbility -> hush $ runParser (NES.toString sAbility) Parser.parseSAbilitySigilBoost)
      # LazyList.catMaybes
      <#> \sigil ->
        { effectType:
            case sigil of
              SigilO -> FilterSigilBoostO
              SigilX -> FilterSigilBoostX
              SigilTriangle -> FilterSigilBoostTriangle
              SigilDiamond -> FilterSigilDiamond
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges: Nothing
            }
        }

  groupsForWeapon' :: ZipList (Maybe GroupEntry)
  groupsForWeapon' = ado
    -- INVARIANT: this assumes weapon effects are listed in the same order at all overboost levels.
    ob0 <- ZipList $ LazyList.fromFoldable weapon.ob0.effects
    ob1 <- ZipList $ LazyList.fromFoldable weapon.ob1.effects
    ob6 <- ZipList $ LazyList.fromFoldable weapon.ob6.effects
    ob10 <- ZipList $ LazyList.fromFoldable weapon.ob10.effects
    in
      groupForWeaponEffect ob0 ob1 ob6 ob10 <#> \{ effectType, potencies, allRanges } ->
        { effectType
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges: Just
                [ { allRanges
                  , allPotencies: potencies
                  }
                ]
            }
        }

  -- If there are many ranges for the same effect (e.g. Arctic Star has PATK Up SingleTarget & PATK Up Self),
  -- this function will merge those `GroupEntry`s (with one `GroupedWeaponRange` each) into a single one (with many `GroupedWeaponRange`s).
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
         , allRanges :: AllRanges
         }
  -- NOTE: an effect can have a different range and different potencies at each
  -- overboost level (e.g. Festive Sword's Enliven is `Self` at OB0/OB1 and `All`
  -- at OB6/OB10), so we read both from every level via `rangeOf` / `potenciesOf`.
  -- Only the `effectType` is determined from the OB0 effect.
  --
  -- This assumes effects are listed in the same order at all overboost levels.
  -- That invariant is enforced by `getDistinctObs` (via `areWeaponEffectsEquivalent`),
  -- which runs for every weapon and crashes on a mismatch.
  groupForWeaponEffect ob0 ob1 ob6 ob10 =
    effectTypeOf <#> \effectType ->
      { effectType
      , potencies: allPotencies
      , allRanges: { ob0: rangeOf ob0, ob1: rangeOf ob1, ob6: rangeOf ob6, ob10: rangeOf ob10 }
      }
    where
    -- `Just` only for effects that have potencies; `Nothing` otherwise.
    allPotencies = do
      ob0Potencies <- potenciesOf ob0
      ob1Potencies <- potenciesOf ob1
      ob6Potencies <- potenciesOf ob6
      ob10Potencies <- potenciesOf ob10
      pure { ob0: ob0Potencies, ob1: ob1Potencies, ob6: ob6Potencies, ob10: ob10Potencies }

    effectTypeOf = case ob0 of
      -- #(ref:heal-threshold)
      Heal { percentage } -> if unwrap percentage >= 30 then Just FilterHeal else Nothing
      Veil {} -> Just FilterVeil
      Provoke {} -> Just FilterProvoke
      Enfeeble {} -> Just FilterEnfeeble
      Stop {} -> Just FilterStop
      ExploitWeakness {} -> Just FilterExploitWeakness
      Enliven {} -> Just FilterEnliven
      HPGain {} -> Just FilterHPGain

      FireWeakness {} -> Just FilterFireWeakness
      IceWeakness {} -> Just FilterIceWeakness
      LightningWeakness {} -> Just FilterLightningWeakness
      EarthWeakness {} -> Just FilterEarthWeakness
      WaterWeakness {} -> Just FilterWaterWeakness
      WindWeakness {} -> Just FilterWindWeakness

      PhysicalWeaponBoost {} -> Just FilterPhysicalWeaponBoost
      MagicWeaponBoost {} -> Just FilterMagicWeaponBoost
      PhysicalDamageBonus {} -> Just FilterPhysicalDamageBonus
      MagicDamageBonus {} -> Just FilterMagicDamageBonus

      FireWeaponBoost {} -> Just FilterFireWeaponBoost
      IceWeaponBoost {} -> Just FilterIceWeaponBoost
      LightningWeaponBoost {} -> Just FilterLightningWeaponBoost
      EarthWeaponBoost {} -> Just FilterEarthWeaponBoost
      WaterWeaponBoost {} -> Just FilterWaterWeaponBoost
      WindWeaponBoost {} -> Just FilterWindWeaponBoost

      FireDamageBonus {} -> Just FilterFireDamageBonus
      IceDamageBonus {} -> Just FilterIceDamageBonus
      LightningDamageBonus {} -> Just FilterLightningDamageBonus
      EarthDamageBonus {} -> Just FilterEarthDamageBonus
      WaterDamageBonus {} -> Just FilterWaterDamageBonus
      WindDamageBonus {} -> Just FilterWindDamageBonus

      PatkUp {} -> Just FilterPatkUp
      MatkUp {} -> Just FilterMatkUp
      PdefUp {} -> Just FilterPdefUp
      MdefUp {} -> Just FilterMdefUp
      FireDamageUp {} -> Just FilterFireDamageUp
      IceDamageUp {} -> Just FilterIceDamageUp
      LightningDamageUp {} -> Just FilterLightningDamageUp
      EarthDamageUp {} -> Just FilterEarthDamageUp
      WaterDamageUp {} -> Just FilterWaterDamageUp
      WindDamageUp {} -> Just FilterWindDamageUp
      FireResistUp {} -> Just FilterFireResistUp
      IceResistUp {} -> Just FilterIceResistUp
      LightningResistUp {} -> Just FilterLightningResistUp
      EarthResistUp {} -> Just FilterEarthResistUp
      WaterResistUp {} -> Just FilterWaterResistUp
      WindResistUp {} -> Just FilterWindResistUp
      EnhanceBuffs {} -> Just FilterEnhanceBuffs

      PatkDown {} -> Just FilterPatkDown
      MatkDown {} -> Just FilterMatkDown
      PdefDown {} -> Just FilterPdefDown
      MdefDown {} -> Just FilterMdefDown
      FireDamageDown {} -> Just FilterFireDamageDown
      IceDamageDown {} -> Just FilterIceDamageDown
      LightningDamageDown {} -> Just FilterLightningDamageDown
      EarthDamageDown {} -> Just FilterEarthDamageDown
      WaterDamageDown {} -> Just FilterWaterDamageDown
      WindDamageDown {} -> Just FilterWindDamageDown
      FireResistDown {} -> Just FilterFireResistDown
      IceResistDown {} -> Just FilterIceResistDown
      LightningResistDown {} -> Just FilterLightningResistDown
      EarthResistDown {} -> Just FilterEarthResistDown
      WaterResistDown {} -> Just FilterWaterResistDown
      WindResistDown {} -> Just FilterWindResistDown
      EnhanceDebuffs {} -> Just FilterEnhanceDebuffs

type ReadCacheResult =
  { userState :: UserState
  , dbMaybe ::
      Maybe
        { db :: Db
        , hasExpired :: Boolean
        }
  }

-- Throws if the cache is empty OR the cache data is corrupted.
readFromCache :: forall m. MonadThrow String m => MonadAff m => m ReadCacheResult
readFromCache = do
  -- NOTE: in V1, we used to store the version number in `db_version`.
  -- From V2 onwards, it's stored in `user_state_version`.
  userStateVersionStr <- lift2 alt (WS.getItem "user_state_version") (WS.getItem "db_version")
    >>= throwOnNothing \_ -> "'user_state_version' / `db_version` not found in cache"
  userStateVersion :: Int <- J.readJSON userStateVersionStr
    # throwOnLeft \err -> "Failed to deserialize 'user_state_version':\n" <> renderJsonErr err

  lastUpdatedStr <- WS.getItem "last_updated"
    >>= throwOnNothing \_ -> "'last_updated' not found in cache"
  lastUpdated :: DateTime <- J.readJSON lastUpdatedStr
    # throwOnLeft \err -> "Failed to deserialize 'last_updated':\n" <> renderJsonErr err

  dbStr <- WS.getItem "db"
    >>= throwOnNothing \_ -> "'db' not found in cache"
  userStateStr <- WS.getItem "user_state" >>= case _ of
    Just userStateStr -> pure userStateStr
    Nothing | userStateVersion == 1 ->
      -- NOTE: in V1, the user state used to be stored in the `db` cache key, so we deserialize it from `dbStr`
      -- From V2 onwards, it's stored in the `user_state` key
      pure dbStr
    Nothing -> do
      throwError "`user_state` not found in cache, even though `db` was found."

  userState :: VLatest.UserState <- parseAndMigrateUserState userStateStr userStateVersion

  dbMaybe <- case fromSerializableDb <$> J.readJSON dbStr of
    Right (db :: Db) -> do
      now <- liftEffect Now.nowDateTime
      pure $ Just
        { db
        , hasExpired: DateTime.diff now lastUpdated > Hours 24.0
        }
    Left err -> do
      Console.error "Failed to deserializable db. The schema may have recently changed."
      Console.error $ renderJsonErr err
      pure Nothing

  pure { userState, dbMaybe }

  where
  fromSerializableDb :: SerializableDb -> Db
  fromSerializableDb db =
    { allWeapons: unwrap db.allWeapons
    , groupedByEffect: unwrap db.groupedByEffect
    , allCharacterNames: unwrap db.allCharacterNames
    }

parseAndMigrateUserState :: forall m. MonadThrow String m => String -> Int -> m VLatest.UserState
parseAndMigrateUserState userStateStr userStateVersion = do
  case userStateVersion of
    1 -> do
      J.readJSON userStateStr
        # throwOnLeft (\err -> "Failed to deserialize db:\n" <> renderJsonErr err)
        <#> V1.deserializeUserState
        <#> the @V1.UserState
        <#> V2.migrate
    2 -> do
      J.readJSON userStateStr
        # throwOnLeft (\err -> "Failed to deserialize user_state:\n" <> renderJsonErr err)
        <#> V2.deserializeUserState
        <#> the @V2.UserState
    _ -> do
      throwError $ "Unexpected user state version number: " <> show userStateVersion

writeToCache :: forall m. MonadAff m => DbState -> m Unit
writeToCache dbState = do
  let dbStr = J.writeJSON $ toSerializableDb dbState.db
  let userStateStr = J.writeJSON $ toSerializableUserState dbState.userState
  lastUpdatedStr <- J.writeJSON <$> liftEffect Now.nowDateTime
  let currentUserStateVersionStr = J.writeJSON currentUserStateVersion

  WS.setItem "db" dbStr
  WS.setItem "user_state" userStateStr
  WS.setItem "last_updated" lastUpdatedStr
  WS.setItem "user_state_version" currentUserStateVersionStr
  where
  toSerializableDb :: Db -> SerializableDb
  toSerializableDb db =
    { allWeapons: MapAsArray db.allWeapons
    , groupedByEffect: MapAsArray db.groupedByEffect
    , allCharacterNames: SetAsArray db.allCharacterNames
    }

toSerializableUserState :: UserState -> SerializableUserState
toSerializableUserState userState =
  { weapons: MapAsArray userState.weapons
  }
