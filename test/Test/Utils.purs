module Test.Utils where

import Core.Weapons.Parser
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Core.Armory as Armory
import Core.Database.VLatest (Armory)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Effect.Aff (Aff, error)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Google.SheetsApi (GetSheetResult)
import Node.Encoding as Node
import Node.FS.Aff as Node
import Node.FS.Perms as Node
import Node.FS.Sync as NodeSync
import Node.Library.Execa (ExecaResult)
import Node.Library.Execa as Node
import Node.Path (FilePath)
import Node.Path as Path
import Parsing (runParser)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))
import Utils as Utils
import Yoga.JSON (class WriteForeign)
import Yoga.JSON as J

shouldEqualPretty :: forall a. WriteForeign a => a -> a -> Aff Unit
shouldEqualPretty actual expected = do
  let actualJson = J.writePrettyJSON 2 actual
  let expectedJson = J.writePrettyJSON 2 expected
  dir <- Node.mkdtemp "test"
  Aff.finally (cleanup dir) do
    let actualFilePath = Path.concat [ dir, "actual.json" ]
    let expectedFilePath = Path.concat [ dir, "expected.json" ]
    Node.writeTextFile Node.UTF8 actualFilePath actualJson
    Node.writeTextFile Node.UTF8 expectedFilePath expectedJson

    cp <- Node.execa "diff" [ "--ignore-trailing-space", "-u", actualFilePath, expectedFilePath ] identity
    result <- cp.getResult

    when (result.exitCode /= Just 0) do
      fail result.stdout
  where
  cleanup dir = do
    Node.rm' dir { force: false, maxRetries: 100, recursive: true, retryDelay: 1000 }

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
    -- Create the parent directory if need be
    let dir = Path.dirname expectedFilePath
    unlessM (liftEffect (NodeSync.exists dir)) do
      Node.mkdir' dir { recursive: false, mode: Node.mkPerms Node.all Node.all Node.all }
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

-- Construct a non-empty string at compile-time.
--
-- Similar to `NES.nes`, but works with visible type applications.
nes :: forall (@a :: Symbol). NES.MakeNonEmpty a => NonEmptyString
nes =
  NES.nes (Proxy :: Proxy a)

loadTestDb :: Aff Armory
loadTestDb = do
  sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
  sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
    Right res -> pure res.values
    Left errs ->
      throwError $ error
        $ "Failed to read `resources/weapons.json`: \n"
            <> Utils.renderJsonErr errs
  let { weapons, errors: _ } = parseWeapons sourceWeapons

  Armory.createArmory weapons Map.empty
