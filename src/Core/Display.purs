module Core.Display where

import Prelude

import Core.Weapons.Search (FilterEffectType(..), FilterRange(..))
import Core.Weapons.Types (CharacterName(..), WeaponName(..))
import Data.Newtype (unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES

class Display a where
  display :: a -> String

instance Display FilterRange where
  display = case _ of
    FilterAll -> "All"
    FilterSingleTargetOrAll -> "Single Target / All"
    FilterSelfOrSingleTargetOrAll -> "Self / Single Target / All"

instance Display FilterEffectType where
  display = case _ of
    FilterHeal -> "Heal"
    -- Buffs
    FilterPatkUp -> "PATK up"
    FilterMatkUp -> "MATK up"
    FilterPdefUp -> "PDEF up"
    FilterMdefUp -> "MDEF up"
    FilterFireDamageUp -> "Fire damage up"
    FilterIceDamageUp -> "Ice damage up"
    FilterThunderDamageUp -> "Thunder damage up"
    FilterEarthDamageUp -> "Earth damage up"
    FilterWaterDamageUp -> "Water damage up"
    FilterWindDamageUp -> "Wind damage up"
    FilterVeil -> "Veil"
    FilterProvoke -> "Provoke"
    -- Debuffs
    FilterPatkDown -> "PATK down"
    FilterMatkDown -> "MATK down"
    FilterPdefDown -> "PDEF down"
    FilterMdefDown -> "MDEF down"
    FilterFireResistDown -> "Fire resist down"
    FilterIceResistDown -> "Ice resist down"
    FilterThunderResistDown -> "Thunder resist down"
    FilterEarthResistDown -> "Earth resist down"
    FilterWaterResistDown -> "Water resist down"
    FilterWindResistDown -> "Wind resist down"

instance Display NonEmptyString where
  display = NES.toString

instance Display WeaponName where
  display = display <<< unwrap

instance Display CharacterName where
  display = display <<< unwrap
