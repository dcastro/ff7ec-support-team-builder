module Test.Core.Weapons.ParserSpec where

import Core.Weapons.Parser
import Core.Weapons.Types
import Prelude
import Test.Spec
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Foreign as Foreign
import Google.SheetsApi (GetSheetResult)
import Node.Encoding as Node
import Node.FS.Aff as Node
import Node.FS.Sync as NodeSync
import Node.Library.Execa (ExecaResult)
import Node.Library.Execa as Node
import Node.Path (FilePath)
import Parsing (runParser)
import Test.Spec.Assertions (fail, shouldEqual)
import Yoga.JSON (class WriteForeign)
import Yoga.JSON as J

spec :: Spec Unit
spec =
  describe "parser" do
    it "parses weapon effects" do
      let
        shouldParse = shouldParse' (parseWeaponEffect { rowId: 0, columnId: 0 })
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
      goldenTest "resources/weapons.snap" weapons

shouldParse' :: forall a m. MonadThrow Error m => Show a => Eq a => Parser a -> String -> a -> m Unit
shouldParse' parser input expectedOutput = case runParser input parser of
  Right res -> res `shouldEqual` expectedOutput
  Left err -> fail $ show err

goldenTest :: forall a. WriteForeign a => FilePath -> a -> Aff Unit
goldenTest expectedFilePath value = do
  -- Serialize the actual value
  let
    actualJson = J.writePrettyJSON 2 value
  -- Check if a snapshot file already exists
  liftEffect (NodeSync.exists expectedFilePath)
    >>= case _ of
        true -> pure unit
        false -> do
          Console.log $ "Snapshot file '" <> expectedFilePath <> "' does not yet exist; creating it..."
          overwriteSnapshotFile actualJson
  result <- shouldEqualFile actualJson expectedFilePath
  when (result.exitCode /= Just 0) do
    -- The snapshot file did not match the expected string.
    -- Update the snapshot file and fail the test.
    overwriteSnapshotFile actualJson
    fail result.stdout
  where
  overwriteSnapshotFile actualJson = do
    Node.writeTextFile Node.UTF8 expectedFilePath (actualJson <> "\n")

-- | Adapted from: https://github.com/jordanmartinez/purescript-spec-golden/blob/v1.0.0/src/Test/Spec/Golden/Assertions.purs#L29-L36
shouldEqualFile :: String -> FilePath -> Aff ExecaResult
shouldEqualFile actual expectedFilePath = do
  cp <- Node.execa "diff" [ "--ignore-trailing-space", "-u", "-", expectedFilePath ] identity
  case cp.stdin of
    Just stdin -> stdin.writeUtf8End actual
    Nothing -> fail "Failed to write `diff`'s stdin"
  result <- cp.getResult
  pure result
