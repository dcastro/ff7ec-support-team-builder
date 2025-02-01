module Test.Core.Weapons.ParserSpec where

import Prelude
import Test.Spec

import Control.Monad.Error.Class (throwError)
import Core.Weapons.Parser
import Core.Database.VLatest
import Data.Either (Either(..))
import Effect.Aff (error)
import Google.SheetsApi (GetSheetResult)
import Node.Encoding as Node
import Node.FS.Aff as Node
import Test.Utils as T
import Utils as Utils
import Yoga.JSON as J

spec :: Spec Unit
spec =
  describe "parser" do
    it "parses weapon effects" do
      let
        shouldParse = T.shouldParse' (parseWeaponEffect { rowId: 0, columnId: 0 })
      "60s Provoke (+0s) [Range: Self]"
        `shouldParse`
          { effectType: Provoke { durExt: { duration: Duration 60, extension: Extension 0 } }
          , range: Self
          }
      "40s 5% Veil (+8s) [Range: Self]"
        `shouldParse`
          { effectType: Veil { durExt: { duration: Duration 40, extension: Extension 8 }, percentage: Percentage 5 }
          , range: Self
          }
      "74% Heal [Range: All Allies]"
        `shouldParse`
          { effectType: Heal { percentage: Percentage 74 }
          , range: All
          }
      "16s PATK Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          { effectType: PatkUp { durExt: { duration: Duration 16, extension: Extension 5 }, potencies: { base: Mid, max: High } }
          , range: All
          }
      "20s PATK Up (+6s) (High) [Range: All Allies]"
        `shouldParse`
          { effectType: PatkUp { durExt: { duration: Duration 20, extension: Extension 6 }, potencies: { base: High, max: High } }
          , range: All
          }
      "20s MDEF Up (+6s) (High) [Range: All Allies] [Condition: Self 70-100% HP]"
        `shouldParse`
          { effectType: MdefUp { durExt: { duration: Duration 20, extension: Extension 6 }, potencies: { base: High, max: High } }
          , range: All
          }
      "26s Ice Damage Up (+8s) (Mid) [Range: Single Ally]"
        `shouldParse`
          { effectType: IceDamageUp { durExt: { duration: Duration 26, extension: Extension 8 }, potencies: { base: Mid, max: Mid } }
          , range: SingleTarget
          }
      "3s Stop (+3s) [Range: Single Enemy] [Condition: First Use]"
        `shouldParse`
          { effectType: Stop { durExt: { duration: Duration 3, extension: Extension 3 } }
          , range: SingleTarget
          }
      "40s Enfeeble (+8s) [Range: Single Enemy]"
        `shouldParse`
          { effectType: Enfeeble { durExt: { duration: Duration 40, extension: Extension 8 } }
          , range: SingleTarget
          }
      "45s 25% WeaknessAttackUp (+9s) [Range: Self]"
        `shouldParse`
          { effectType: ExploitWeakness { durExt: { duration: Duration 45, extension: Extension 9 }, percentage: Percentage 25 }
          , range: Self
          }
    it "parses all weapons" do
      sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
      sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
        Right res -> pure res.values
        Left errs ->
          throwError $ error
            $ "Failed to read `resources/weapons.json`: \n"
                <> Utils.renderJsonErr errs
      let parseResult = parseWeapons sourceWeapons
      T.goldenTest "snaps/weapons.snap" parseResult
