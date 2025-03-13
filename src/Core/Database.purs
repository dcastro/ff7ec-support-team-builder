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
    table <- liftAff $ SheetsApi.getSheet "Weapons!A:Z"

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
      ThunderDamageUp { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          ThunderDamageUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
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
      ThunderResistDown { range: range1, durExt: _, potencies: pot1 } ->
        case y of
          ThunderResistDown { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
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

      FireWeakness { range: range1, durExt: _, percentage: _ } ->
        case y of
          FireWeakness { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit
      IceWeakness { range: range1, durExt: _, percentage: _ } ->
        case y of
          IceWeakness { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit
      ThunderWeakness { range: range1, durExt: _, percentage: _ } ->
        case y of
          ThunderWeakness { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit
      EarthWeakness { range: range1, durExt: _, percentage: _ } ->
        case y of
          EarthWeakness { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit
      WaterWeakness { range: range1, durExt: _, percentage: _ } ->
        case y of
          WaterWeakness { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit
      WindWeakness { range: range1, durExt: _, percentage: _ } ->
        case y of
          WindWeakness { range: range2, durExt: _, percentage: _ } -> range1 == range2
          _ -> crash unit

  crash _ = unsafeCrashWith $ "Effects for weapon " <> display weapon.name <> " are not in the same order"

createDbState :: forall m. MonadEffect m => MonadRec m => Array Weapon -> UserState -> m DbState
createDbState newWeapons existingUserState = do
  (finalDb :: Db) <- Arr.foldRecM
    (\db weapon -> insertWeapon weapon db)
    newDb
    newWeapons

  -- If new weapons were added to the db, we need to create "empty states" for them.
  let
    (finalUserState :: UserState) =
      F.foldl
        ( \userState weaponData ->
            case Map.lookup weaponData.weapon.name userState.weapons of
              Just _ -> userState
              Nothing -> do
                let
                  updatedUserStateWeapons =
                    Map.insert weaponData.weapon.name
                      { ignored: false
                      , ownedOb: Just $ NAR.last weaponData.distinctObs
                      }
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

groupsForWeapon :: Weapon -> List.List GroupEntry
groupsForWeapon weapon = do
  mergeRanges $ Arr.fold
    [ LazyList.catMaybes (unwrap groupsForWeapon')
    , getCureAllAbility
    , getCommandAbilityDiamondSigil
    , getSigilBoost
    ]
  where
  getCureAllAbility :: LazyList.List GroupEntry
  getCureAllAbility = do
    let sAbilities = [ weapon.sAbilities.slot1, weapon.sAbilities.slot2, weapon.sAbilities.slot3 ]
    if Arr.any Parser.hasCureAllSAbility sAbilities then
      LazyList.singleton
        { effectType: FilterHeal
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges: Just
                [ { range: All
                  , allPotencies: Nothing
                  }
                ]
            }
        }
    else
      LazyList.nil

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
      groupForWeaponEffect ob0 ob1 ob6 ob10 <#> \{ effectType, potencies, range } -> do
        let
          ranges =
            case range of
              Just range ->
                Just
                  [ { range
                    , allPotencies: potencies
                    }
                  ]
              Nothing -> Nothing

        { effectType
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges
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

  -- INVARIANT: this function assumes an effect has the same range at all overboost levels,
  -- so it just returns the range for the effect at OB0.
  groupForWeaponEffect
    :: WeaponEffect
    -> WeaponEffect
    -> WeaponEffect
    -> WeaponEffect
    -> Maybe
         { effectType :: FilterEffectType
         , potencies :: Maybe AllPotencies
         , range :: Maybe Range
         }
  groupForWeaponEffect ob0 ob1 ob6 ob10 = do
    case ob0 of
      Heal { range, percentage } ->
        if unwrap percentage >= 35 then Just { effectType: FilterHeal, range: Just range, potencies: Nothing }
        else Nothing
      Veil { range } -> Just { effectType: FilterVeil, range: Just range, potencies: Nothing }
      Provoke { range } -> Just { effectType: FilterProvoke, range: Just range, potencies: Nothing }
      Enfeeble { range } -> Just { effectType: FilterEnfeeble, range: Just range, potencies: Nothing }
      Stop { range } -> Just { effectType: FilterStop, range: Just range, potencies: Nothing }
      ExploitWeakness { range } -> Just { effectType: FilterExploitWeakness, range: Just range, potencies: Nothing }

      FireWeakness { range } -> Just { effectType: FilterFireWeakness, range: Just range, potencies: Nothing }
      IceWeakness { range } -> Just { effectType: FilterIceWeakness, range: Just range, potencies: Nothing }
      ThunderWeakness { range } -> Just { effectType: FilterThunderWeakness, range: Just range, potencies: Nothing }
      EarthWeakness { range } -> Just { effectType: FilterEarthWeakness, range: Just range, potencies: Nothing }
      WaterWeakness { range } -> Just { effectType: FilterWaterWeakness, range: Just range, potencies: Nothing }
      WindWeakness { range } -> Just { effectType: FilterWindWeakness, range: Just range, potencies: Nothing }

      PatkUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        PatkUp ob1, PatkUp ob6, PatkUp ob10 -> Just { effectType: FilterPatkUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      MatkUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        MatkUp ob1, MatkUp ob6, MatkUp ob10 -> Just { effectType: FilterMatkUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      PdefUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        PdefUp ob1, PdefUp ob6, PdefUp ob10 -> Just { effectType: FilterPdefUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      MdefUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        MdefUp ob1, MdefUp ob6, MdefUp ob10 -> Just { effectType: FilterMdefUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      FireDamageUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        FireDamageUp ob1, FireDamageUp ob6, FireDamageUp ob10 -> Just { effectType: FilterFireDamageUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      IceDamageUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        IceDamageUp ob1, IceDamageUp ob6, IceDamageUp ob10 -> Just { effectType: FilterIceDamageUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      ThunderDamageUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        ThunderDamageUp ob1, ThunderDamageUp ob6, ThunderDamageUp ob10 -> Just { effectType: FilterThunderDamageUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      EarthDamageUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        EarthDamageUp ob1, EarthDamageUp ob6, EarthDamageUp ob10 -> Just { effectType: FilterEarthDamageUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      WaterDamageUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        WaterDamageUp ob1, WaterDamageUp ob6, WaterDamageUp ob10 -> Just { effectType: FilterWaterDamageUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      WindDamageUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        WindDamageUp ob1, WindDamageUp ob6, WindDamageUp ob10 -> Just { effectType: FilterWindDamageUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      PatkDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        PatkDown ob1, PatkDown ob6, PatkDown ob10 -> Just { effectType: FilterPatkDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      MatkDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        MatkDown ob1, MatkDown ob6, MatkDown ob10 -> Just { effectType: FilterMatkDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      PdefDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        PdefDown ob1, PdefDown ob6, PdefDown ob10 -> Just { effectType: FilterPdefDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      MdefDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        MdefDown ob1, MdefDown ob6, MdefDown ob10 -> Just { effectType: FilterMdefDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      FireResistDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        FireResistDown ob1, FireResistDown ob6, FireResistDown ob10 -> Just { effectType: FilterFireResistDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      IceResistDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        IceResistDown ob1, IceResistDown ob6, IceResistDown ob10 -> Just { effectType: FilterIceResistDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      ThunderResistDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        ThunderResistDown ob1, ThunderResistDown ob6, ThunderResistDown ob10 -> Just { effectType: FilterThunderResistDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      EarthResistDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        EarthResistDown ob1, EarthResistDown ob6, EarthResistDown ob10 -> Just { effectType: FilterEarthResistDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      WaterResistDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        WaterResistDown ob1, WaterResistDown ob6, WaterResistDown ob10 -> Just { effectType: FilterWaterResistDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit
      WindResistDown { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
        WindResistDown ob1, WindResistDown ob6, WindResistDown ob10 -> Just { effectType: FilterWindResistDown, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
        _, _, _ -> crash unit

  crash _ = unsafeCrashWith $ "Effects for weapon " <> display weapon.name <> " are not in the same order"

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
