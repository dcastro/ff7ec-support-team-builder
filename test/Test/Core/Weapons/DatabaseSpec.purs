module Test.Core.Weapons.DatabaseSpec where

import Core.Weapons.Parser2
import Prelude
import Test.Spec

import Control.Monad.Error.Class (throwError)
import Core.Database as Db
import Core.Database.VLatest2
import Data.Array as Arr
import Data.Either (Either(..))
import Data.List.Lazy as LazyList
import Data.List.ZipList (ZipList(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (error)
import Google.SheetsApi (GetSheetResult)
import Node.Encoding as Node
import Node.FS.Aff as Node
import Test.Utils2 as T
import Utils (MapAsArray(..))
import Utils as Utils
import Yoga.JSON as J

spec :: Spec Unit
spec =
  describe "database" do
    it "groups weapons" do
      sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
      sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
        Right res -> pure res.values
        Left errs ->
          throwError $ error
            $ "Failed to read `resources/weapons.json`: \n"
                <> Utils.renderJsonErr errs
      let { weapons, errors: _ } = parseWeapons sourceWeapons

      db <- Db.createDb weapons Map.empty

      T.goldenTest "snaps/grouped_weapons.snap" $ MapAsArray db.groupedByEffect

    it "check differences in OB potencies" do
      -- A script to find out whether all weapons have the same effects at OB0 as they do at OB1,
      -- and the same effects at OB6 as they do at OB10.
      sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
      sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
        Right res -> pure res.values
        Left errs ->
          throwError $ error
            $ "Failed to read `resources/weapons.json`: \n"
                <> Utils.renderJsonErr errs
      let { weapons, errors: _ } = parseWeapons sourceWeapons

      db <- Db.createDb weapons Map.empty

      let
        obDiffs =
          Map.values db.allWeapons
            # Arr.fromFoldable
            <#> (\weaponData -> groupEffects weaponData.weapon)
            >>= makeDiff

      T.goldenTest "snaps/ob-differences.snap" $ obDiffs

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
