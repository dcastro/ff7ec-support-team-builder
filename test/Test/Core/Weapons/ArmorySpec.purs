module Test.Core.Weapons.ArmorySpec where

import Core.Weapons.Parser
import Prelude
import Test.Spec

import Control.Monad.Error.Class (throwError)
import Core.Armory (ArmoryWeapon)
import Core.Armory as Armory
import Core.Weapons.Types (EffectType(..), WeaponEffect, WeaponName)
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
import Test.Utils as T
import Utils (MapAsArray(..))
import Utils as Utils
import Yoga.JSON as J

spec :: Spec Unit
spec =
  describe "armory" do
    it "groups weapons" do
      sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
      sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
        Right res -> pure res.values
        Left errs ->
          throwError $ error
            $ "Failed to read `resources/weapons.json`: \n"
                <> Utils.renderJsonErr errs
      let { weapons, errors: _ } = parseWeapons sourceWeapons

      armory <- Armory.createArmory weapons Map.empty

      T.goldenTest "resources/grouped_weapons.snap" $ MapAsArray armory.groupedByEffect

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

      armory <- Armory.createArmory weapons Map.empty

      let
        obDiffs =
          Map.values armory.allWeapons
            # Arr.fromFoldable
            <#> groupEffects
            >>= makeDiff

      T.goldenTest "snaps/ob-differences.snap" $ obDiffs

makeDiff :: WeaponInfo -> Array ObDiff
makeDiff { name, effects } =
  effects >>= \effect ->
    case effect.ob0.effectType of
      Heal _ -> []
      _ ->
        Arr.catMaybes
          [ if effect.ob0 /= effect.ob1 then Just
              { weaponName: name
              , obX: "OB0"
              , obY: "OB1"
              , obXEffect: effect.ob0
              , obYEffect: effect.ob1
              }
            else
              Nothing
          , if effect.ob6 /= effect.ob10 then Just
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

groupEffects :: ArmoryWeapon -> WeaponInfo
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
