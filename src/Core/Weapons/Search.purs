module Core.Weapons.Search where

import Core.Weapons.Types
import Prelude

import Data.Array as Arr
import Data.Foldable as F
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty as NES
import Utils as Utils
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum

data FilterRange
  = FilterAll
  | FilterSingleTargetOrAll
  | FilterSelfOrSingleTargetOrAll

derive instance Generic FilterRange _
derive instance Eq FilterRange
derive instance Ord FilterRange
instance Show FilterRange where
  show = genericShow

instance WriteForeign FilterRange where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance ReadForeign FilterRange where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

allFilterRanges :: Array FilterRange
allFilterRanges = Utils.listEnum

data FilterEffectType
  = FilterHeal
  -- Buffs
  | FilterVeil
  | FilterProvoke
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

derive instance Generic FilterEffectType _
derive instance Eq FilterEffectType
derive instance Ord FilterEffectType
instance Show FilterEffectType where
  show = genericShow

instance WriteForeign FilterEffectType where
  writeImpl = J.genericWriteForeignEnum Enum.defaultOptions

instance ReadForeign FilterEffectType where
  readImpl = J.genericReadForeignEnum Enum.defaultOptions

allFilterEffectTypes :: Array FilterEffectType
allFilterEffectTypes = Utils.listEnum

type Filter =
  { effectType :: FilterEffectType
  , range :: FilterRange
  }

type FilterResult =
  { filter :: Filter
  , required :: Boolean
  , matchingWeapons :: Array Weapon
  }

type Combination = Array
  { filter :: Filter
  , weapon :: Maybe Weapon
  }

combinations :: Array FilterResult -> Array Combination
combinations results =
  Arr.foldr
    ( \(filterResult :: FilterResult) (combinations :: Array Combination) -> do
        (weapon :: Maybe Weapon) <- filterResult.matchingWeapons # handleOptional filterResult.required
        (combination :: Combination) <- combinations
        [ Arr.cons { filter: filterResult.filter, weapon } combination ]
    )
    ([ [] ] :: Array Combination)
    results
  where

  -- If there are no matching weaopns, return an array that yields a single `None`
  handleOptional :: Boolean -> Array Weapon -> Array (Maybe Weapon)
  handleOptional required matchingWeapons =
    if not required && matchingWeapons == [] then [ Nothing ]
    else Just <$> matchingWeapons

findMatchingWeapons :: Filter -> Array Weapon -> Array Weapon
findMatchingWeapons filter weapons = weapons # Arr.filter (matches filter)

-- Check if a weapon matches a given filter.
matches :: Filter -> Weapon -> Boolean
matches filter weapon =
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

type Character =
  { name :: CharacterName
  , mainHand :: EquipedWeapon
  , offHand :: Maybe EquipedWeapon
  }

type EquipedWeapon =
  { weapon :: Weapon
  -- The filters that this weapon matched on.
  , matchedFilters :: Array Filter
  }

type AssignmentResult =
  { -- | Characters indexed by their name.
    characters :: Map String Character
  ,
    -- | A list of the effects for which no weapon was found.
    missedFilters :: Array Filter
  }

-- Attempts to equip the selected weapons to, at maximum, `n` characters.
--
-- Returns `Nothing` if:
--  * There are more than 2 weapons for any character.
--  * The selected weapons belong to more than `n` characters.
assignWeaponsToCharacters :: Int -> Combination -> Maybe AssignmentResult
assignWeaponsToCharacters maxCharacterCount =
  Arr.foldRecM
    ( \assignments { filter, weapon } ->
        case weapon of
          Nothing ->
            -- No weapons found for this filter
            Just $ assignments { missedFilters = Arr.cons filter assignments.missedFilters }
          Just weapon -> do
            let
              characterName = weapon.character :: CharacterName
              characterName' = NES.toString (unwrap characterName)
            updatedCharacters <-
              case Map.lookup characterName' assignments.characters of
                Nothing ->
                  -- This character hasn't been created yet, so we attempt to create it.
                  if Map.size assignments.characters >= maxCharacterCount then Nothing
                  else Just $ Map.insert characterName' (mkCharacter characterName weapon filter) assignments.characters
                Just existingCharacter -> do
                  -- This character already exists, so we attempt to equip this weapon.
                  updatedCharacter <- equipWeapon weapon filter existingCharacter
                  pure $ Map.insert characterName' updatedCharacter assignments.characters
            Just $ assignments { characters = updatedCharacters }
    )
    { characters: Map.empty, missedFilters: [] }

  where

  mkCharacter :: CharacterName -> Weapon -> Filter -> Character
  mkCharacter name mainHandWeapon matchedFilter =
    { name
    , mainHand:
        { weapon: mainHandWeapon
        , matchedFilters: [ matchedFilter ]
        }
    , offHand: Nothing
    }

  equipWeapon :: Weapon -> Filter -> Character -> Maybe Character
  equipWeapon weapon filter character = do
    -- If this weapon is already equiped in the main hand, simply update `matchedFilters`
    if character.mainHand.weapon.name == weapon.name then Just character { mainHand = addMatchedFilter filter character.mainHand }
    else
      case character.offHand of
        Just offHand | offHand.weapon.name == weapon.name -> do
          -- If this weapon is already equiped in the off hand, simply update `matchedFilters`
          pure $ character { offHand = Just $ addMatchedFilter filter offHand }
        Just _ ->
          -- If both hands are already equiped, we can't equip any more weapons - this function fails.
          Nothing
        Nothing ->
          -- The off hand is free, we can equip this weapon in the off hand.
          pure $ character
            { offHand = Just
                { weapon
                , matchedFilters: [ filter ]
                }
            }

  addMatchedFilter :: Filter -> EquipedWeapon -> EquipedWeapon
  addMatchedFilter matchedFilter weapon =
    weapon { matchedFilters = Arr.cons matchedFilter weapon.matchedFilters }

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

cartesianProduct :: forall a. Array (Array a) -> Array (Array a)
cartesianProduct arr =
  case Arr.uncons arr of
    Nothing -> []
    Just { head, tail: [] } -> do
      x <- head
      [ [ x ] ]
    Just { head, tail } -> do
      x <- head
      xs <- cartesianProduct tail
      [ Arr.cons x xs ]

cartesianProduct2 :: forall a. Array (Array a) -> Array (Array a)
cartesianProduct2 =
  Arr.foldr
    ( \arr b -> do
        x <- arr
        xs <- b
        [ Arr.cons x xs ]
    )
    [ [] ]
