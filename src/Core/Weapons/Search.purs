module Core.Weapons.Search where

import Core.Weapons.Types
import Prelude

import Core.Armory (Armory, ArmoryWeapon, Filter, GroupedWeapon)
import Core.Display (display)
import Data.Array as Arr
import Data.Foldable as F
import Data.Function (on)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord.Down (Down(..))
import Data.String.NonEmpty as NES
import Utils (unsafeFromJust)

type FilterResult =
  { filter :: Filter
  , matchingWeapons :: Array FilterResultWeapon
  }

type FilterResultWeapon =
  { weapon :: ArmoryWeapon
  , potenciesAtOb10 :: Maybe Potencies
  }

type Combination = Array CombinationItem

type CombinationItem =
  { filter :: Filter
  , weapon :: ArmoryWeapon
  , potenciesAtOb10 :: Maybe Potencies
  }

findMatchingWeapons :: Filter -> Armory -> FilterResult
findMatchingWeapons filter armory = do
  let
    matchingGroupedWeapons = Map.lookup filter armory.groupedByEffect # fromMaybe [] :: Array GroupedWeapon

    matchingWeapons = matchingGroupedWeapons <#> \{ weaponName, potenciesAtOb10 } -> do
      let weapon = Map.lookup weaponName armory.allWeapons `unsafeFromJust` ("Weapon name '" <> display weaponName <> "' from group '" <> show filter <> "' not found.")
      { weapon
      , potenciesAtOb10
      }
  { filter
  , matchingWeapons
  }

search :: Int -> Array Filter -> Armory -> Array AssignmentResult
search maxCharacterCount filters armory = do
  let filterResults = filters <#> \filter -> findMatchingWeapons filter armory

  let combs = combinations filterResults :: Array Combination

  combs
    # Arr.mapMaybe (assignWeaponsToCharacters maxCharacterCount)
    # Arr.sortBy (comparing $ scoreTeam >>> Down)

combinations :: Array FilterResult -> Array Combination
combinations results =
  results
    # Arr.nubBy (compare `on` _.filter)
    # Arr.foldr
        ( \(filterResult :: FilterResult) (combinations :: Array Combination) -> do
            { weapon, potenciesAtOb10 } <- filterResult.matchingWeapons
              # discardIgnored
            (combination :: Combination) <- combinations

            let combinationItem = { filter: filterResult.filter, weapon, potenciesAtOb10 }

            [ Arr.cons combinationItem combination ]
        )
        ([ [] ] :: Array Combination)
  where
  discardIgnored :: Array FilterResultWeapon -> Array FilterResultWeapon
  discardIgnored = Arr.filter \{ weapon } -> not weapon.ignored

type AssignmentResult =
  { -- | Characters indexed by their name.
    characters :: Map String Character
  }

type Character =
  { name :: CharacterName
  , mainHand :: EquipedWeapon
  , offHand :: Maybe EquipedWeapon
  }

type EquipedWeapon =
  { weapon :: ArmoryWeapon
  -- The filters that this weapon matched on.
  , matchedFilters :: Array EquipedWeaponFilter
  }

type EquipedWeaponFilter =
  { filter :: Filter
  , potenciesAtOb10 :: Maybe Potencies
  }

-- Attempts to equip the selected weapons to, at maximum, `n` characters.
--
-- Returns `Nothing` if:
--  * There are more than 2 weapons for any character.
--  * The selected weapons belong to more than `n` characters.
assignWeaponsToCharacters :: Int -> Combination -> Maybe AssignmentResult
assignWeaponsToCharacters maxCharacterCount combs =
  combs
    # Arr.foldRecM
        ( \assignments { filter, weapon, potenciesAtOb10 } ->
            assignWeapon { filter, potenciesAtOb10 } weapon assignments
        )
        { characters: Map.empty }
  where

  -- Returns `Nothing` if:
  --  * There are more than 2 weapons for any character.
  --  * The selected weapons belong to more than `n` characters.
  assignWeapon :: EquipedWeaponFilter -> ArmoryWeapon -> AssignmentResult -> Maybe AssignmentResult
  assignWeapon filter weapon assignments = do
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

  mkCharacter :: CharacterName -> ArmoryWeapon -> EquipedWeaponFilter -> Character
  mkCharacter name mainHandWeapon matchedFilter =
    { name
    , mainHand:
        { weapon: mainHandWeapon
        , matchedFilters: [ matchedFilter ]
        }
    , offHand: Nothing
    }

  equipWeapon :: ArmoryWeapon -> EquipedWeaponFilter -> Character -> Maybe Character
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

  addMatchedFilter :: EquipedWeaponFilter -> EquipedWeapon -> EquipedWeapon
  addMatchedFilter matchedFilter weapon =
    weapon { matchedFilters = Arr.cons matchedFilter weapon.matchedFilters }

getEquipedWeapons :: Character -> Array EquipedWeapon
getEquipedWeapons char =
  case char.offHand of
    Just offHand -> [ char.mainHand, offHand ]
    Nothing -> [ char.mainHand ]

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
    # Arr.mapMaybe _.potenciesAtOb10

  characterCount = Map.size characters

  weaponCount = Map.values characters <#> (\char -> Arr.length $ getEquipedWeapons char) # F.sum

  scorePotency :: Potency -> Int
  scorePotency = case _ of
    Low -> 1
    Mid -> 2
    High -> 3
    ExtraHigh -> 4
