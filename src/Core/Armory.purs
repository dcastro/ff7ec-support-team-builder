module Core.Armory where

import Core.Weapons.Types
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Error.Class as M
import Control.Monad.Except (runExceptT)
import Core.Weapons.Parser as P
import Core.WebStorage as WS
import Data.Array as Arr
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), hush)
import Data.Foldable as Fold
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Time.Duration (Hours(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Foreign (MultipleErrors, renderForeignError)
import Google.SheetsApi as SheetsApi
import Record as Record
import Type.Proxy (Proxy(..))
import Utils (whenJust)
import Yoga.JSON as J

type Armory = Array ArmoryWeapon

type ArmoryWeapon =
  { name :: WeaponName
  , character :: CharacterName
  , image :: NonEmptyString
  , ob0 :: ObLevel
  , ob1 :: ObLevel
  , ob6 :: ObLevel
  , ob10 :: ObLevel
  , cureAllAbility :: Boolean
  , owned :: Boolean
  }

init :: Aff (Maybe Armory)
init = do
  runExceptT readFromCache >>= case _ of
    Left _ -> do
      armoryMb <- hush <$> runExceptT loadFromSpreadsheet
      whenJust armoryMb $ writeToCache
      pure armoryMb
    Right { armory, hasExpired } | hasExpired -> do
      hush <$> runExceptT (updateArmory armory) >>= case _ of
        Just updatedArmory -> do
          writeToCache updatedArmory
          pure $ Just updatedArmory
        Nothing -> do
          Console.log "Failed to updated armory"
          pure $ Just armory
    Right { armory, hasExpired: _ } -> do
      pure $ Just armory
  where

  -- Load the weapons from the spreadsheet, and updating the existing armory.
  updateArmory :: forall m. MonadAff m => MonadThrow Unit m => Armory -> m Armory
  updateArmory existingArmory = do
    newArmory <- loadFromSpreadsheet

    -- Convert the array to a map for faster lookups
    let
      existingArmory' =
        Arr.foldl
          (\armory weapon -> Map.insert weapon.name weapon armory)
          Map.empty
          existingArmory
          :: Map WeaponName ArmoryWeapon

    updatedArmory <- Arr.foldM
      ( \armory newWeapon -> do
          case Map.lookup newWeapon.name armory of
            Nothing -> do
              Console.log $ "Weapon added: " <> show newWeapon.name
              pure $ Map.insert newWeapon.name newWeapon armory
            Just existingWeapon -> do
              Console.log $ "Weapon updated: " <> show newWeapon.name
              let newWeapon' = newWeapon { owned = existingWeapon.owned }
              pure $ Map.insert newWeapon.name newWeapon' armory
      )
      existingArmory'
      newArmory

    -- Convert the map back to an array
    pure $ Map.values updatedArmory # Arr.fromFoldable

  -- Throws if we can't parse the Google Sheet.
  loadFromSpreadsheet :: forall m. MonadAff m => MonadThrow Unit m => m Armory
  loadFromSpreadsheet = do
    table <- liftAff $ SheetsApi.getSheet "Weapons!A:Z"
    weapons <- P.parseWeapons table.result.values `logOnLeft`
      \err -> "Failed to parse weapons:\n" <> err
    let
      armory = weapons
        -- Keep only weapons which have effects (buffs, debuffs, or heal).
        # Arr.filter
            ( \weapon ->
                not (Arr.null weapon.ob10.effects) || weapon.cureAllAbility
            )
        -- Set the `owned` field to `false` by default.
        # map \weapon -> Record.insert (Proxy :: Proxy "owned") false weapon
    pure armory

-- Throws if the cache is empty OR the cache data is corrupted.
readFromCache :: forall m. MonadThrow Unit m => MonadAff m => m { armory :: Armory, hasExpired :: Boolean }
readFromCache = do
  armoryStr <- throwOnNothing $ WS.getItem "armory"
  lastUpdatedStr <- throwOnNothing $ WS.getItem "last_updated"

  armory :: Armory <- J.readJSON armoryStr `logOnLeft` \err ->
    "Failed to deserialize armory:\n" <> renderJsonErr err
  lastUpdated :: DateTime <- J.readJSON lastUpdatedStr `logOnLeft` \err ->
    "Failed to deserialize armory:\n" <> renderJsonErr err

  now <- liftEffect Now.nowDateTime
  let hasExpired = DateTime.diff now lastUpdated > Hours 24.0

  pure { armory, hasExpired }

writeToCache :: forall m. MonadAff m => Armory -> m Unit
writeToCache armory = do
  let armoryStr = J.writeJSON armory
  lastUpdatedStr <- J.writeJSON <$> liftEffect Now.nowDateTime

  WS.setItem "armory" armoryStr
  WS.setItem "last_updated" lastUpdatedStr

throwOnNothing :: forall m a. MonadThrow Unit m => m (Maybe a) -> m a
throwOnNothing action =
  action >>= M.liftMaybe unit

logOnNothing' :: forall m a. MonadThrow Unit m => MonadEffect m => String -> m (Maybe a) -> m a
logOnNothing' msg action = do
  mb <- action
  case mb of
    Just a -> pure a
    Nothing -> do
      Console.log msg
      throwError unit

logOnNothing :: forall m a. MonadThrow Unit m => MonadEffect m => String -> Maybe a -> m a
logOnNothing msg mb = do
  case mb of
    Just a -> pure a
    Nothing -> do
      Console.log msg
      throwError unit

logOnLeft :: forall m e a. MonadThrow Unit m => MonadEffect m => Either e a -> (e -> String) -> m a
logOnLeft either mkMsg = do
  case either of
    Right a -> pure a
    Left err -> do
      Console.log $ mkMsg err
      throwError unit

renderJsonErr :: MultipleErrors -> String
renderJsonErr errs =
  Fold.intercalate ", " (renderForeignError <$> errs)
