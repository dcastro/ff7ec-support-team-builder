module Test.Utils where

import Core.Weapons.Parser
import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
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
