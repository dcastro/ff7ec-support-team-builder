module Core.Weapons.Search where

import Prelude
import Core.Weapons.Types
import Data.Foldable as F

data FilterRange
  = FilterAll
  | FilterSingleTargetOrAll
  | FilterSelfOrSingleTargetOrAll

derive instance Eq FilterRange

data FilterEffectType
  = FilterHeal
  -- Buffs
  | FilterPatkUp
  | FilterMatkUp
  | FilterPdefUp
  | FilterMdefUp
  | FilterFireDamageUp
  | FilterIceDamageUp
  | FilterThunderDamageUp
  | FilterEarthDamageUp
  | FilterWaterDamageUp
  | FilterWindDamageUp
  | FilterVeil
  | FilterProvoke
  -- Debuffs
  | FilterPatkDown
  | FilterMatkDown
  | FilterPdefDown
  | FilterMdefDown
  | FilterFireResistDown
  | FilterIceResistDown
  | FilterThunderResistDown
  | FilterEarthResistDown
  | FilterWaterResistDown
  | FilterWindResistDown

derive instance Eq FilterEffectType

type Filter =
  { effectType :: FilterEffectType
  , range :: FilterRange
  }

-- Check if a weapon matches a given filter.
matches :: Weapon -> Filter -> Boolean
matches weapon filter =
  weaponHasHealAll
    || weaponHasEffect
  where
  weaponHasEffect =
    weapon.ob10.effects
      # F.any \effect ->
          matchEffectType effect || matchRange effect

  weaponHasHealAll = filter.effectType == FilterHeal && filter.range == FilterAll && weapon.cureAllAbility

  matchEffectType :: WeaponEffect -> Boolean
  matchEffectType weaponEffect = case weaponEffect.effectType of
    Heal _ -> filter.effectType == FilterHeal
    -- Buffs
    PatkUp _ -> filter.effectType == FilterPatkUp
    MatkUp _ -> filter.effectType == FilterMatkUp
    PdefUp _ -> filter.effectType == FilterPdefUp
    MdefUp _ -> filter.effectType == FilterMdefUp
    FireDamageUp _ -> filter.effectType == FilterFireDamageUp
    IceDamageUp _ -> filter.effectType == FilterIceDamageUp
    ThunderDamageUp _ -> filter.effectType == FilterThunderDamageUp
    EarthDamageUp _ -> filter.effectType == FilterEarthDamageUp
    WaterDamageUp _ -> filter.effectType == FilterWaterDamageUp
    WindDamageUp _ -> filter.effectType == FilterWindDamageUp
    Veil -> filter.effectType == FilterVeil
    Provoke -> filter.effectType == FilterProvoke
    -- Debuffs
    PatkDown _ -> filter.effectType == FilterPatkDown
    MatkDown _ -> filter.effectType == FilterMatkDown
    PdefDown _ -> filter.effectType == FilterPdefDown
    MdefDown _ -> filter.effectType == FilterMdefDown
    FireResistDown _ -> filter.effectType == FilterFireResistDown
    IceResistDown _ -> filter.effectType == FilterIceResistDown
    ThunderResistDown _ -> filter.effectType == FilterThunderResistDown
    EarthResistDown _ -> filter.effectType == FilterEarthResistDown
    WaterResistDown _ -> filter.effectType == FilterWaterResistDown
    WindResistDown _ -> filter.effectType == FilterWindResistDown

  matchRange :: WeaponEffect -> Boolean
  matchRange weaponEffect = case filter.range, weaponEffect.range of
    -- Filter by All
    FilterAll, Self -> false
    FilterAll, SingleTarget -> false
    FilterAll, All -> true
    -- Filter by Single Target or better
    FilterSingleTargetOrAll, Self -> false
    FilterSingleTargetOrAll, SingleTarget -> true
    FilterSingleTargetOrAll, All -> true
    -- Filter by Self or better
    FilterSelfOrSingleTargetOrAll, Self -> true
    FilterSelfOrSingleTargetOrAll, SingleTarget -> true
    FilterSelfOrSingleTargetOrAll, All -> true

defaultFilterRange :: FilterEffectType -> FilterRange
defaultFilterRange = case _ of
  FilterHeal -> FilterAll
  -- Buffs
  FilterPatkUp -> FilterAll
  FilterMatkUp -> FilterAll
  FilterPdefUp -> FilterAll
  FilterMdefUp -> FilterAll
  FilterFireDamageUp -> FilterSingleTargetOrAll
  FilterIceDamageUp -> FilterSingleTargetOrAll
  FilterThunderDamageUp -> FilterSingleTargetOrAll
  FilterEarthDamageUp -> FilterSingleTargetOrAll
  FilterWaterDamageUp -> FilterSingleTargetOrAll
  FilterWindDamageUp -> FilterSingleTargetOrAll
  FilterVeil -> FilterSelfOrSingleTargetOrAll
  FilterProvoke -> FilterSelfOrSingleTargetOrAll
  -- Debuffs
  FilterPatkDown -> FilterSingleTargetOrAll
  FilterMatkDown -> FilterSingleTargetOrAll
  FilterPdefDown -> FilterSingleTargetOrAll
  FilterMdefDown -> FilterSingleTargetOrAll
  FilterFireResistDown -> FilterSingleTargetOrAll
  FilterIceResistDown -> FilterSingleTargetOrAll
  FilterThunderResistDown -> FilterSingleTargetOrAll
  FilterEarthResistDown -> FilterSingleTargetOrAll
  FilterWaterResistDown -> FilterSingleTargetOrAll
  FilterWindResistDown -> FilterSingleTargetOrAll
