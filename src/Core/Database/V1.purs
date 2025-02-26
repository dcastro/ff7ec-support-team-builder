module Core.Database.V1 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString)
import Utils (MapAsArray)
import Yoga.JSON (class ReadForeign)
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum

type UserState =
  { weapons :: Map WeaponName UserStateWeapon
  }

type UserStateWeapon =
  { ignored :: Boolean
  , ownedOb :: Maybe ObRange
  }

type Db =
  { allWeapons :: Map WeaponName WeaponData
  }

collectUserState :: Db -> UserState
collectUserState db =
  { weapons: db.allWeapons
  }

type WeaponData =
  {
    -- NOTE: phased out the `ignored` feature,
    -- but keeping the `ignored` flag in the db in case I want to bring it back
    ignored :: Boolean
  , ownedOb :: Maybe ObRange
  }

type SerializableDb =
  { allWeapons :: MapAsArray WeaponName WeaponData
  }

newtype WeaponName = WeaponName NonEmptyString

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

instance ReadForeign FromOb where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

instance ReadForeign ToOb where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

derive newtype instance ReadForeign WeaponName
derive newtype instance ReadForeign ObRange
