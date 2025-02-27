module Test.Core.Weapons.DatabaseSpec where

import Core.Database.Types
import Core.Database.UserState.VLatest
import Prelude
import Test.Spec

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Core.Database as Database
import Core.Display (display)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.List.Lazy as LazyList
import Data.List.ZipList (ZipList(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Node.Encoding as Node
import Node.FS.Aff as Node
import Test.Utils as T
import Test.Utils as Utils
import Utils (MapAsArray(..), unsafeFromJust)

spec :: Spec Unit
spec =
  describe "database" do
    it "builds weapon data" do
      dbState <- Utils.loadTestDbState

      let
        weaponDataAndState = dbState.db.allWeapons
          <#> \wd -> do
            let weaponState = Map.lookup wd.weapon.name dbState.userState.weapons `unsafeFromJust` "Weapon found in db but not in user state"
            { ignored: weaponState.ignored
            , distinctObs: wd.distinctObs <#> display
            , ownedOb: weaponState.ownedOb <#> display
            }

      T.goldenTest "snaps/weapon_data.snap" $ MapAsArray weaponDataAndState
    it "groups weapons" do
      dbState <- Utils.loadTestDbState

      T.goldenTest "snaps/grouped_weapons.snap" $ MapAsArray dbState.db.groupedByEffect

    it "check differences in OB potencies" do
      -- A script to find out whether all weapons have the same effects at OB0 as they do at OB1,
      -- and the same effects at OB6 as they do at OB10.
      dbState <- Utils.loadTestDbState

      let
        obDiffs =
          Map.values dbState.db.allWeapons
            # Arr.fromFoldable
            <#> (\weaponData -> groupEffects weaponData.weapon)
            >>= makeDiff

      T.goldenTest "snaps/ob-differences.snap" $ obDiffs

    it "migrates user state" do
      userStateV1Str <- Node.readTextFile Node.UTF8 "resources/user-state-v1.json"
      runExceptT (Database.parseAndMigrateUserState userStateV1Str 1) >>= case _ of
        Left err -> throwError $ error err
        Right userState -> do
          T.goldenTest "snaps/user_state_vlatest.snap" $ Database.toSerializableUserState userState

makeDiff :: WeaponInfo -> Array ObDiff
makeDiff { name, effects } =
  effects >>= \effect ->
    Arr.catMaybes
      [ if getPotencies effect.ob0.effectType /= getPotencies effect.ob1.effectType then Just
          { weaponName: name
          , obX: "OB0"
          , obY: "OB1"
          , obXEffect: effect.ob0
          , obYEffect: effect.ob1
          }
        else
          Nothing
      , if getPotencies effect.ob6.effectType /= getPotencies effect.ob10.effectType then Just
          { weaponName: name
          , obX: "OB6"
          , obY: "OB10"
          , obXEffect: effect.ob6
          , obYEffect: effect.ob10
          }
        else
          Nothing
      ]

type ObDiff =
  { weaponName :: WeaponName
  , obX :: String
  , obY :: String
  , obXEffect :: WeaponEffect
  , obYEffect :: WeaponEffect
  }

type WeaponInfo =
  { name :: WeaponName
  , effects :: Array EffectInfo
  }

type EffectInfo =
  { ob0 :: WeaponEffect
  , ob1 :: WeaponEffect
  , ob6 :: WeaponEffect
  , ob10 :: WeaponEffect
  }

groupEffects :: Weapon -> WeaponInfo
groupEffects weapon =
  { name: weapon.name
  , effects: Arr.fromFoldable effects
  }
  where
  ZipList effects = ado
    ob0 <- ZipList $ LazyList.fromFoldable weapon.ob0.effects
    ob1 <- ZipList $ LazyList.fromFoldable weapon.ob1.effects
    ob6 <- ZipList $ LazyList.fromFoldable weapon.ob6.effects
    ob10 <- ZipList $ LazyList.fromFoldable weapon.ob10.effects
    in
      { ob0
      , ob1
      , ob6
      , ob10
      }

-- Similar to `EffectType`, but we discard all information except potencies.
data EffectTypeAndPotencies
  = PatkUp' Potencies
  | MatkUp' Potencies
  | PdefUp' Potencies
  | MdefUp' Potencies
  | FireDamageUp' Potencies
  | IceDamageUp' Potencies
  | ThunderDamageUp' Potencies
  | EarthDamageUp' Potencies
  | WaterDamageUp' Potencies
  | WindDamageUp' Potencies
  | PatkDown' Potencies
  | MatkDown' Potencies
  | PdefDown' Potencies
  | MdefDown' Potencies
  | FireResistDown' Potencies
  | IceResistDown' Potencies
  | ThunderResistDown' Potencies
  | EarthResistDown' Potencies
  | WaterResistDown' Potencies
  | WindResistDown' Potencies

derive instance Eq EffectTypeAndPotencies

getPotencies :: EffectType -> Maybe EffectTypeAndPotencies
getPotencies = case _ of
  Heal {} -> Nothing
  PatkUp { potencies } -> Just $ PatkUp' potencies
  MatkUp { potencies } -> Just $ MatkUp' potencies
  PdefUp { potencies } -> Just $ PdefUp' potencies
  MdefUp { potencies } -> Just $ MdefUp' potencies
  FireDamageUp { potencies } -> Just $ FireDamageUp' potencies
  IceDamageUp { potencies } -> Just $ IceDamageUp' potencies
  ThunderDamageUp { potencies } -> Just $ ThunderDamageUp' potencies
  EarthDamageUp { potencies } -> Just $ EarthDamageUp' potencies
  WaterDamageUp { potencies } -> Just $ WaterDamageUp' potencies
  WindDamageUp { potencies } -> Just $ WindDamageUp' potencies
  Veil {} -> Nothing
  Provoke {} -> Nothing
  PatkDown { potencies } -> Just $ PatkDown' potencies
  MatkDown { potencies } -> Just $ MatkDown' potencies
  PdefDown { potencies } -> Just $ PdefDown' potencies
  MdefDown { potencies } -> Just $ MdefDown' potencies
  FireResistDown { potencies } -> Just $ FireResistDown' potencies
  IceResistDown { potencies } -> Just $ IceResistDown' potencies
  ThunderResistDown { potencies } -> Just $ ThunderResistDown' potencies
  EarthResistDown { potencies } -> Just $ EarthResistDown' potencies
  WaterResistDown { potencies } -> Just $ WaterResistDown' potencies
  WindResistDown { potencies } -> Just $ WindResistDown' potencies
  Enfeeble {} -> Nothing
  Stop {} -> Nothing
  ExploitWeakness {} -> Nothing
