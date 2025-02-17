module Core.Weapons.Search where

import Core.Database.VLatest
import Prelude

import Core.Display (class Display, display)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NAR
import Data.Foldable as F
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Ord.Down (Down(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty as NES
import Utils (unsafeFromJust)
import Utils as Utils
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Generics as J
import Yoga.JSON.Generics.EnumSumRep as Enum

type Filter =
  { effectType :: FilterEffectType
  , range :: FilterRange
  , minBasePotency :: Potency
  , minMaxPotency :: Potency
  }

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

instance Display FilterRange where
  display = case _ of
    FilterAll -> "All"
    FilterSingleTargetOrAll -> "Single Target / All"
    FilterSelfOrSingleTargetOrAll -> "Self / Single Target / All"

allFilterRanges :: Array FilterRange
allFilterRanges = Utils.listEnum

type FilterResult =
  { filter :: Filter
  , matchingWeapons :: Array FilterResultWeapon
  }

type FilterResultWeapon =
  { weapon :: WeaponData
  -- For a given effect, the potencies this weapon has for that effect,
  -- at the owned overboost level.
  , potencies :: Maybe Potencies
  , matchesFilters :: Boolean
  }

findMatchingWeapons :: Filter -> Db -> FilterResult
findMatchingWeapons filter db = do
  let
    (weaponsForEffectType :: Array GroupedWeapon) = Map.lookup filter.effectType db.groupedByEffect # fromMaybe []

    (matchingWeapons :: Array FilterResultWeapon) = weaponsForEffectType
      # Arr.mapMaybe \{ weaponName, ranges } -> do
          let
            weapon = Map.lookup weaponName db.allWeapons
              `unsafeFromJust` ("Weapon name '" <> display weaponName <> "' from group '" <> show filter.effectType <> "' not found.")

          -- Throw out weapons that don't match the required range.
          matchingRanges <- matchRanges filter.range ranges

          let
            potencies =
              case weapon.ownedOb of
                Nothing -> Nothing
                Just ownedOb -> do
                  selectBestPotencies ownedOb matchingRanges

            matchesFilters =
              -- NOTE: phased out the `ignored` feature,
              -- but keeping the `ignored` flag in the db in case I want to bring it back
              -- not weapon.ignored &&
              isJust weapon.ownedOb && hasMinPotencies potencies

          Just
            { weapon
            , potencies
            , matchesFilters
            }

  { filter
  , matchingWeapons
  }

  where

  hasMinPotencies :: Maybe Potencies -> Boolean
  hasMinPotencies =
    case _ of
      Nothing ->
        -- Either:
        --   * this weapon is not owned (in which case, it doesn't matter what we return)
        --   * or the selected effect type is not associated with potencies, e.g. "Heal" has no potency
        --     (in which case, we resort to vacuous truth)
        true
      Just potencies ->
        potencies.base >= filter.minBasePotency
          &&
            potencies.max >= filter.minMaxPotency

  selectBestPotencies :: ObRange -> NonEmptyArray GroupedWeaponRange -> Maybe Potencies
  selectBestPotencies ownedOb ranges = do
    ranges
      # NAR.toArray
      # Arr.mapMaybe (selectPotenciesForOwnedOb ownedOb)
      # F.maximumBy (compare `on` _.max <> compare `on` _.base)

  selectPotenciesForOwnedOb :: ObRange -> GroupedWeaponRange -> Maybe Potencies
  selectPotenciesForOwnedOb (ObRange ownedOb) range =
    case range.allPotencies of
      Nothing -> Nothing
      Just allPotencies ->
        case ownedOb.from of
          FromOb0 -> Just allPotencies.ob0
          FromOb1 -> Just allPotencies.ob1
          FromOb6 -> Just allPotencies.ob6
          FromOb10 -> Just allPotencies.ob10

  matchRanges :: FilterRange -> Array GroupedWeaponRange -> Maybe (NonEmptyArray GroupedWeaponRange)
  matchRanges filterRange ranges =
    ranges
      # Arr.filter (\r -> matchRange filterRange r.range)
      # NAR.fromArray

  matchRange :: FilterRange -> Range -> Boolean
  matchRange =
    case _, _ of
      FilterAll, All -> true
      FilterAll, SingleTarget -> false
      FilterAll, Self -> false

      FilterSingleTargetOrAll, All -> true
      FilterSingleTargetOrAll, SingleTarget -> true
      FilterSingleTargetOrAll, Self -> false

      FilterSelfOrSingleTargetOrAll, All -> true
      FilterSelfOrSingleTargetOrAll, SingleTarget -> true
      FilterSelfOrSingleTargetOrAll, Self -> true

applyFilters :: Array Filter -> Db -> Array FilterResult
applyFilters filters db = filters <#> \filter -> findMatchingWeapons filter db

search :: Int -> Array FilterResult -> Array AssignmentResult
search maxCharacterCount filterResults =
  if Arr.null filterResults then []
  else do
    filterResults
      # Arr.nubBy (compare `on` _.filter)
      # Arr.foldr
          ( \(filterResult :: FilterResult) (teams :: Array AssignmentResult) -> do
              { weapon, potencies } <- filterResult.matchingWeapons
                # discardUnmatched

              (team :: AssignmentResult) <- teams

              case assignWeapon maxCharacterCount { filter: filterResult.filter, potencies } weapon team of
                Just team -> [ team ]
                Nothing -> []
          )
          ([ emptyTeam ] :: Array AssignmentResult)
      # Arr.sortBy (comparing $ scoreTeam >>> Down)
  where
  discardUnmatched :: Array FilterResultWeapon -> Array FilterResultWeapon
  discardUnmatched = Arr.filter \filterResultWeapon -> filterResultWeapon.matchesFilters

emptyTeam :: AssignmentResult
emptyTeam = { characters: Map.empty }

type AssignmentResult =
  { -- | Characters indexed by their name.
    characters :: Map String Character
  }

type Character =
  { name :: CharacterName
  , mainHand :: Maybe EquipedWeapon
  , offHand :: Maybe EquipedWeapon
  }

type EquipedWeapon =
  { weaponData :: WeaponData
  -- The filters that this weapon matched on.
  , matchedFilters :: Array EquipedWeaponFilter
  }

type EquipedWeaponFilter =
  { filter :: Filter
  , potencies :: Maybe Potencies
  }

-- Returns `Nothing` if:
--  * There are more than 2 weapons for any character.
--  * The selected weapons belong to more than `n` characters.
assignWeapon :: Int -> EquipedWeaponFilter -> WeaponData -> AssignmentResult -> Maybe AssignmentResult
assignWeapon maxCharacterCount filter weaponData assignments = do
  let
    characterName = weaponData.weapon.character :: CharacterName
    characterName' = NES.toString (unwrap characterName)
  updatedCharacters <-
    case Map.lookup characterName' assignments.characters of
      Nothing ->
        -- This character hasn't been created yet, so we attempt to create it.
        if Map.size assignments.characters >= maxCharacterCount then Nothing
        else Just $ Map.insert characterName' (mkCharacter characterName weaponData filter) assignments.characters
      Just existingCharacter -> do
        -- This character already exists, so we attempt to equip this weapon.
        updatedCharacter <- equipWeapon weaponData filter existingCharacter
        pure $ Map.insert characterName' updatedCharacter assignments.characters
  Just $ assignments { characters = updatedCharacters }
  where

  mkCharacter :: CharacterName -> WeaponData -> EquipedWeaponFilter -> Character
  mkCharacter name mainHandWeapon matchedFilter =
    { name
    , mainHand: Just
        { weaponData: mainHandWeapon
        , matchedFilters: [ matchedFilter ]
        }
    , offHand: Nothing
    }

  equipWeapon :: WeaponData -> EquipedWeaponFilter -> Character -> Maybe Character
  equipWeapon weaponData filter character = do
    case character.mainHand of
      Nothing -> Just character
        { mainHand = Just
            { weaponData
            , matchedFilters: [ filter ]
            }
        }
      Just mainHand ->
        if mainHand.weaponData.weapon.name == weaponData.weapon.name
        -- If this weapon is already equiped in the main hand, simply update `matchedFilters`
        then Just character { mainHand = Just $ addMatchedFilter filter mainHand }
        else
          case character.offHand of
            Just offHand | offHand.weaponData.weapon.name == weaponData.weapon.name -> do
              -- If this weapon is already equiped in the off hand, simply update `matchedFilters`
              pure $ character { offHand = Just $ addMatchedFilter filter offHand }
            Just _ ->
              -- If both hands are already equiped, we can't equip any more weapons - this function fails.
              Nothing
            Nothing ->
              -- The off hand is free, we can equip this weapon in the off hand.
              pure $ character
                { offHand = Just
                    { weaponData
                    , matchedFilters: [ filter ]
                    }
                }

  addMatchedFilter :: EquipedWeaponFilter -> EquipedWeapon -> EquipedWeapon
  addMatchedFilter matchedFilter weapon =
    weapon { matchedFilters = Arr.cons matchedFilter weapon.matchedFilters }

getEquipedWeapons :: Character -> Array EquipedWeapon
getEquipedWeapons char =
  Arr.fromFoldable char.mainHand <> Arr.fromFoldable char.offHand

getTeamWeaponNames :: AssignmentResult -> Set WeaponName
getTeamWeaponNames team =
  Map.values team.characters
    # Arr.fromFoldable
    >>= getEquipedWeapons
    <#> (\equipedWeapon -> equipedWeapon.weaponData.weapon.name)
    # Set.fromFoldable

getCharacterNames :: AssignmentResult -> Set CharacterName
getCharacterNames team =
  Map.values team.characters
    <#> (\char -> char.name)
    # Set.fromFoldable

data TeamScore = TeamScore
  { maxPotenciesScore :: Int
  , basePotenciesScore :: Int
  , characterCountScore :: Int
  , weaponCountScore :: Int
  }

derive instance Eq TeamScore

instance Ord TeamScore where
  compare (TeamScore x) (TeamScore y) = go x y
    where
    go =
      -- NOTE: the order of these checks determines their precedence.
      -- I.e. criteria are listed in order from most to least important.
      Arr.fold
        [ compare `on` _.maxPotenciesScore
        , compare `on` _.basePotenciesScore
        , compare `on` _.characterCountScore
        , compare `on` _.weaponCountScore
        ]

scoreTeam :: AssignmentResult -> TeamScore
scoreTeam { characters } = do
  TeamScore
    { maxPotenciesScore: allPotencies <#> (\pots -> scorePotency pots.max) # F.sum
    , basePotenciesScore: allPotencies <#> (\pots -> scorePotency pots.base) # F.sum
    , characterCountScore: negate characterCount
    , weaponCountScore: negate weaponCount
    }
  where
  allPotencies = Arr.fromFoldable (Map.values characters)
    >>= getEquipedWeapons
    >>= _.matchedFilters
    # Arr.mapMaybe (\equipedWeapon -> equipedWeapon.potencies)

  characterCount = Map.size characters

  weaponCount = Map.values characters <#> (\char -> Arr.length $ getEquipedWeapons char) # F.sum

  scorePotency :: Potency -> Int
  scorePotency = case _ of
    Low -> 1
    Mid -> 2
    High -> 3
    ExtraHigh -> 4

filterMustHaveChars :: Set CharacterName -> Array AssignmentResult -> Array AssignmentResult
filterMustHaveChars mustHaveChars teams =
  teams
    # Arr.filter \team -> mustHaveChars `Set.subset` getCharacterNames team

-- This is used to filter out teams where they may have been assigned the same weapons, but for different roles.
-- E.g. selecting Patk Up All + Matk Up All + Heal All, would return:
--     * Crimson Staff (Matk Up All + Heal All) + Kamura Wand (Patk Up All)
--     * Crimson Staff (Matk Up All) + Kamura Wand (Patk Up All + Heal All)
--
-- Displaying both is redundant, so this function will eliminate one of them.
-- TODO: return the one with the better score
filterDuplicates :: Array AssignmentResult -> Array AssignmentResult
filterDuplicates =
  Arr.nubBy (compare `on` getTeamWeaponNames)
