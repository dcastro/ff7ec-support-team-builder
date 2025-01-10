module Core.Armory where

import Core.Weapons.Types
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Core.Weapons.Parser as P
import Core.WebStorage as WS
import Data.Array as Arr
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Time.Duration (Hours(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Google.SheetsApi as SheetsApi
import Record as Record
import Type.Proxy (Proxy(..))
import Utils (logOnLeft, renderJsonErr, throwOnNothing, whenJust)
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

init :: forall m. MonadAff m => m (Maybe Armory)
init = do
  runExceptT readFromCache >>= case _ of
    Left _ -> do
      Console.log "Armory not found in cache, loading it from the spreadsheet..."
      armoryMb <- hush <$> runExceptT loadFromSpreadsheet
      whenJust armoryMb $ writeToCache
      pure armoryMb
    Right { armory, hasExpired } | hasExpired -> do
      Console.log "Armory found in cache but has expired, updating cache..."
      hush <$> runExceptT (updateArmory armory) >>= case _ of
        Just updatedArmory -> do
          writeToCache updatedArmory
          pure $ Just updatedArmory
        Nothing -> do
          Console.log "Failed to updated armory"
          pure $ Just armory
    Right { armory, hasExpired: _ } -> do
      Console.log "Armory found in cache."
      pure $ Just armory
  where

  -- Load the weapons from the spreadsheet, and updating the existing armory.
  updateArmory :: forall f. MonadAff f => MonadThrow Unit f => Armory -> f Armory
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
  loadFromSpreadsheet :: forall f. MonadAff f => MonadThrow Unit f => f Armory
  loadFromSpreadsheet = do
    table <- liftAff $ SheetsApi.getSheet "Weapons!A:Z"

    let { weapons, errors } = P.parseWeapons table.result.values
    for_ errors \err -> Console.log $ "Failed to parse weapon:\n" <> err

    when (Arr.null weapons) do
      Console.log "Failed to parse any weapons"
      throwError unit
    let
      armory = weapons
        -- Keep only weapons which have effects (buffs, debuffs, or heal).
        # Arr.filter
            ( \weapon ->
                not (Arr.null weapon.ob10.effects) || weapon.cureAllAbility
            )
        -- Set the `owned` field to `true` by default.
        # map \weapon -> Record.insert (Proxy :: Proxy "owned") true weapon
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
