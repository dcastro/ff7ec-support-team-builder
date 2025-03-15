module Test.Core.Weapons.SerializationSpec where

import Prelude
import Test.Spec

import Core.Database.Types (FilterEffectType, WeaponEffect)
import Core.Database.Types as Types
import Data.Foldable (for_)
import Test.Spec.Assertions (shouldEqual)
import Yoga.JSON as Json

spec :: Spec Unit
spec =
  describe "serialization" do
    it "FilerEffectType roundtrips" do
      for_ Types.allFilterEffectTypes \filterEffectType -> do
        let
          (result :: _ FilterEffectType) = filterEffectType
            # Json.writeJSON
            # Json.readJSON

        pure filterEffectType `shouldEqual` result

    it "WeaponEffect roundtrips" do
      for_ Types.exhaustiveWeaponEffectMatch \weaponEffect -> do
        let
          (result :: _ WeaponEffect) = weaponEffect
            # Json.writeJSON
            # Json.readJSON

        pure weaponEffect `shouldEqual` result
