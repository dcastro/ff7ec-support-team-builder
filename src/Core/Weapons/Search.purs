module Core.Weapons.Search where

import Core.Weapons.Types
import Prelude

import Core.Armory (ArmoryWeapon, Filter, Armory)
import Core.Display (display)
import Data.Array as Arr
import Data.Function (on)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String.NonEmpty as NES
import Utils (unsafeFromJust)

type FilterResult =
  { filter :: Filter
  , required :: Boolean
  , matchingWeapons :: Array ArmoryWeapon
  }

type FilterOpts =
  { filter :: Filter
  , required :: Boolean
  }

type Combination = Array
  { filter :: Filter
  , weapon :: Maybe ArmoryWeapon
  }

findMatchingWeapons :: Filter -> Armory -> Array ArmoryWeapon
findMatchingWeapons filter armory = do
  let matchingWeaponNames = Map.lookup filter armory.groupedByEffect # fromMaybe [] :: Array WeaponName
  matchingWeaponNames <#> \weaponName ->
    Map.lookup weaponName armory.allWeapons `unsafeFromJust` ("Weapon name '" <> display weaponName <> "' from group '" <> show filter <> "' not found.")

search :: Array FilterOpts -> Armory -> Array AssignmentResult
search filterOpts armory = do
  let
    filterResults = filterOpts <#> \{ filter, required } ->
      { filter
      , required
      , matchingWeapons: findMatchingWeapons filter armory
      } :: FilterResult

  let combs = combinations filterResults :: Array Combination

  combs # Arr.mapMaybe (assignWeaponsToCharacters 2)

combinations :: Array FilterResult -> Array Combination
combinations results =
  results
    # Arr.nubBy (compare `on` _.filter)
    # Arr.foldr
        ( \(filterResult :: FilterResult) (combinations :: Array Combination) -> do
            (weapon :: Maybe ArmoryWeapon) <- filterResult.matchingWeapons
              # discardIgnored
              # handleOptional filterResult.required
            (combination :: Combination) <- combinations
            [ Arr.cons { filter: filterResult.filter, weapon } combination ]
        )
        ([ [] ] :: Array Combination)
  where

  -- If there are no matching weaopns, return an array that yields a single `None`
  handleOptional :: Boolean -> Array ArmoryWeapon -> Array (Maybe ArmoryWeapon)
  handleOptional required matchingWeapons =
    if not required && matchingWeapons == [] then [ Nothing ]
    else Just <$> matchingWeapons

  discardIgnored :: Array ArmoryWeapon -> Array ArmoryWeapon
  discardIgnored = Arr.filter \weapon -> not weapon.ignored

type Character =
  { name :: CharacterName
  , mainHand :: EquipedWeapon
  , offHand :: Maybe EquipedWeapon
  }

type EquipedWeapon =
  { weapon :: ArmoryWeapon
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

  mkCharacter :: CharacterName -> ArmoryWeapon -> Filter -> Character
  mkCharacter name mainHandWeapon matchedFilter =
    { name
    , mainHand:
        { weapon: mainHandWeapon
        , matchedFilters: [ matchedFilter ]
        }
    , offHand: Nothing
    }

  equipWeapon :: ArmoryWeapon -> Filter -> Character -> Maybe Character
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

getEquipedWeapons :: Character -> Array EquipedWeapon
getEquipedWeapons char =
  case char.offHand of
    Just offHand -> [ char.mainHand, offHand ]
    Nothing -> [ char.mainHand ]
