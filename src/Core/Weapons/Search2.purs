module Core.Weapons.Search2 where

import Core.Database.VLatest2
import Prelude

import Core.Display (display)
import Data.Array as Arr
import Data.Foldable as F
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord.Down (Down(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty as NES
import Utils (unsafeFromJust)

type Filter =
  { effectType :: FilterEffectType
  , range :: FilterRange
  }

data FilterRange
  = FilterAll
  | FilterSingleTargetOrAll
  | FilterSelfOrSingleTargetOrAll

derive instance Generic FilterRange _
derive instance Eq FilterRange
derive instance Ord FilterRange

type FilterResult =
  { filter :: Filter
  , matchingWeapons :: Array FilterResultWeapon
  }

type FilterResultWeapon =
  { weapon :: WeaponData
  , allPotencies :: Maybe AllPotencies
  }

findMatchingWeapons :: Filter -> Db -> FilterResult
findMatchingWeapons filter db = do
  let
    (weaponsForEffectType :: Array GroupedWeapon) = Map.lookup filter.effectType db.groupedByEffect # fromMaybe []

    (matchingWeapons :: Array FilterResultWeapon) = weaponsForEffectType
      # Arr.mapMaybe \{ weaponName, range, allPotencies } -> do
          let
            weapon = Map.lookup weaponName db.allWeapons
              `unsafeFromJust` ("Weapon name '" <> display weaponName <> "' from group '" <> show filter.effectType <> "' not found.")

          if matchRange filter.range range then Just
            { weapon
            , allPotencies

            }
          else Nothing
  { filter
  , matchingWeapons
  }

  where
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
              { weapon, allPotencies } <- filterResult.matchingWeapons
                # discardIgnored

              (team :: AssignmentResult) <- teams

              case assignWeapon maxCharacterCount { filter: filterResult.filter, allPotencies } weapon team of
                Just team -> [ team ]
                Nothing -> []
          )
          ([ emptyTeam ] :: Array AssignmentResult)
      # Arr.sortBy (comparing $ scoreTeam >>> Down)
  where
  discardIgnored :: Array FilterResultWeapon -> Array FilterResultWeapon
  discardIgnored = Arr.filter \{ weapon } -> not weapon.ignored

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
  , allPotencies :: Maybe AllPotencies
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
    # Arr.mapMaybe (\equipedWeapon -> equipedWeapon.allPotencies <#> _.ob10)

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
