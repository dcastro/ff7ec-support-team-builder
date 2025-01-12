module Test.Core.Weapons.ArmorySpec where

import Core.Weapons.Parser
import Prelude
import Test.Spec

import Control.Monad.Error.Class (throwError)
import Core.Armory as Armory
import Data.Array as Arr
import Data.Either (Either(..))
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

      armory <- Arr.foldRecM
        (\armory weapon -> Armory.insertWeapon weapon armory)
        Armory.newArmory
        weapons

      T.goldenTest "resources/grouped_weapons.snap" $ MapAsArray (armory.groupedByEffect)
