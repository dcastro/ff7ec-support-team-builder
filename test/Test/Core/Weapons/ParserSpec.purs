module Test.Core.Weapons.ParserSpec where

import Core.Weapons.Parser
import Core.Weapons.Types
import Prelude
import Test.Spec
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Effect.Aff (error)
import Foreign as Foreign
import Google.SheetsApi (GetSheetResult)
import Node.Encoding as Node
import Node.FS.Aff as Node
import Yoga.JSON as J
import Test.Utils as T

spec :: Spec Unit
spec =
  describe "parser" do
    it "parses weapon effects" do
      let
        shouldParse = T.shouldParse' (parseWeaponEffect { rowId: 0, columnId: 0 })
      "60s Provoke (+0s) [Range: Self]"
        `shouldParse`
          { effectType: Provoke
          , range: Self
          }
      "40s 5% Veil (+8s) [Range: Self]"
        `shouldParse`
          { effectType: Veil
          , range: Self
          }
      "74% Heal [Range: All Allies]"
        `shouldParse`
          { effectType: Heal { percentage: Percentage 74 }
          , range: All
          }
      "16s PATK Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          { effectType: PatkUp { base: Mid, max: High }
          , range: All
          }
      "20s PATK Up (+6s) (High) [Range: All Allies]"
        `shouldParse`
          { effectType: PatkUp { base: High, max: High }
          , range: All
          }
      "20s MDEF Up (+6s) (High) [Range: All Allies] [Condition: Self 70-100% HP]"
        `shouldParse`
          { effectType: MdefUp { base: High, max: High }
          , range: All
          }
      "26s Ice Damage Up (+8s) (Mid) [Range: Single Ally]"
        `shouldParse`
          { effectType: IceDamageUp { base: Mid, max: Mid }
          , range: SingleTarget
          }
    it "parses all weapons" do
      sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
      sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
        Right res -> pure res.values
        Left errs ->
          throwError $ error
            $ "Failed to read `resources/weapons.json`: "
            <> (errs # foldMap \err -> "\n" <> Foreign.renderForeignError err)
      weapons <- case parseWeapons sourceWeapons of
        Right res -> pure res
        Left err -> throwError $ error err
      T.goldenTest "resources/weapons.snap" weapons
