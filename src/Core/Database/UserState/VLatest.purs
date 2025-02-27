module Core.Database.UserState.VLatest where

import Prelude

import Core.Database.UserState.V1 as Prev
import Core.Display (class Display, display)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.Tuple.Nested ((/\))
import Safe.Coerce (coerce)
import Utils (MapAsArray, the)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum

deserializeUserState :: SerializableUserState -> UserState
deserializeUserState userState =
  { weapons: unwrap userState.weapons
  }

migrate :: Prev.UserState -> UserState
migrate prev =
  { weapons: prev.weapons
      # Map.toUnfoldable
      # the @(Array _)
      <#> (\(k /\ v) -> (coerce k /\ (migrateUserStateWeapon v)))
      # Map.fromFoldable
  }
  where
  migrateUserStateWeapon :: Prev.UserStateWeapon -> UserStateWeapon
  migrateUserStateWeapon prev =
    { ignored: prev.ignored
    , ownedOb: migrateObRange <$> prev.ownedOb
    }

  migrateObRange :: Prev.ObRange -> ObRange
  migrateObRange (Prev.ObRange prev) = ObRange
    { from: migrateFromOb prev.from
    , to: migrateToOb prev.to
    }

  migrateFromOb :: Prev.FromOb -> FromOb
  migrateFromOb = case _ of
    Prev.FromOb0 -> FromOb0
    Prev.FromOb1 -> FromOb1
    Prev.FromOb6 -> FromOb6
    Prev.FromOb10 -> FromOb10

  migrateToOb :: Prev.ToOb -> ToOb
  migrateToOb = case _ of
    Prev.ToOb0 -> ToOb0
    Prev.ToOb5 -> ToOb5
    Prev.ToOb9 -> ToOb9
    Prev.ToOb10 -> ToOb10

type SerializableUserState =
  { weapons :: MapAsArray WeaponName UserStateWeapon
  }

type UserState =
  { weapons :: Map WeaponName UserStateWeapon
  }

newtype WeaponName = WeaponName NonEmptyString

type UserStateWeapon =
  { ignored :: Boolean
  -- | The OB range owned by the user. E.g. if the user has this weapon at OB3, this range could be OB0-5.
  -- INVARIANT: this needs to match one of the items in the corresponding `WeaponData.distinctObs`.
  , ownedOb :: Maybe ObRange
  }

newtype ObRange = ObRange
  { from :: FromOb
  , to :: ToOb
  }

data FromOb = FromOb0 | FromOb1 | FromOb6 | FromOb10
data ToOb = ToOb0 | ToOb5 | ToOb9 | ToOb10

derive instance Generic FromOb _
derive instance Generic ToOb _

derive instance Eq FromOb
derive instance Eq ToOb
derive newtype instance Eq WeaponName
derive newtype instance Eq ObRange

derive instance Ord FromOb
derive instance Ord ToOb
derive newtype instance Ord WeaponName
derive newtype instance Ord ObRange

derive instance Newtype WeaponName _

instance Show FromOb where
  show = genericShow

instance Show ToOb where
  show = genericShow

derive newtype instance Show WeaponName
derive newtype instance Show ObRange

instance WriteForeign FromOb where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance WriteForeign ToOb where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

derive newtype instance WriteForeign WeaponName
derive newtype instance WriteForeign ObRange

instance ReadForeign FromOb where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign ToOb where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

derive newtype instance ReadForeign WeaponName
derive newtype instance ReadForeign ObRange

instance Display WeaponName where
  display = display <<< unwrap

instance Display ObRange where
  display (ObRange { from, to }) = do
    let fromStr = displayFrom from
    let toStr = displayTo to
    if fromStr == toStr then "OB" <> displayFrom from
    else "OB" <> displayFrom from <> "-" <> displayTo to

    where
    displayFrom :: FromOb -> String
    displayFrom = case _ of
      FromOb0 -> "0"
      FromOb1 -> "1"
      FromOb6 -> "6"
      FromOb10 -> "10"

    displayTo :: ToOb -> String
    displayTo = case _ of
      ToOb0 -> "0"
      ToOb5 -> "5"
      ToOb9 -> "9"
      ToOb10 -> "10"
