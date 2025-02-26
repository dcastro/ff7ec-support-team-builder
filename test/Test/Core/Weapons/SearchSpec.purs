module Test.Core.Weapons.SearchSpec (spec) where

import Core.Database.VLatest
import Prelude
import Test.Spec
import Test.Utils

import Control.Monad.Error.Class (throwError)
import Core.Weapons.Search (AssignmentResult, Character, Filter, FilterRange(..), FilterResultWeapon, emptyTeam)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Array.NonEmpty as NAR
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils as T

spec :: Spec Unit
spec =
  describe "search" do
    combinationsSpec
    assignWeaponSpec
    searchExamplesSpec
    findMatchingWeaponsSpec

combinationsSpec :: Spec Unit
combinationsSpec = do
  describe "combinations" do
    let
      weapon11 = mkWeapon (nes @"11") aerith
      weapon12 = mkWeapon (nes @"12") aerith
      weapon13 = mkWeapon (nes @"13") aerith
      weapon21 = mkWeapon (nes @"21") matt
      weapon22 = mkWeapon (nes @"22") matt
      weapon31 = mkWeapon (nes @"31") lucia

      potencies11 = Just { base: Low, max: Low }
      potencies12 = Just { base: Low, max: Mid }
      potencies13 = Just { base: Low, max: High }
      potencies21 = Just { base: Mid, max: Mid }
      potencies22 = Just { base: Mid, max: High }
      potencies31 = Just { base: High, max: High }

    it "finds all possible combinations" do
      let
        teams = Search.search 3 Set.empty
          [ { filter: filter1
            , matchingWeapons:
                [ { weapon: weapon11, weaponState: defaultWeaponState, potencies: potencies11, matchesFilters: true }
                , { weapon: weapon12, weaponState: defaultWeaponState, potencies: potencies12, matchesFilters: true }
                , { weapon: weapon13, weaponState: defaultWeaponState, potencies: potencies13, matchesFilters: true }
                ]
            }
          , { filter: filter2
            , matchingWeapons:
                [ { weapon: weapon21, weaponState: defaultWeaponState, potencies: potencies21, matchesFilters: true }
                , { weapon: weapon22, weaponState: defaultWeaponState, potencies: potencies22, matchesFilters: true }
                ]
            }
          , { filter: filter3
            , matchingWeapons:
                [ { weapon: weapon31, weaponState: defaultWeaponState, potencies: potencies31, matchesFilters: true }
                ]
            }
          ]

      setAsArray teams `shouldEqualPretty` setAsArray
        [ { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon11 filter1 potencies11
              , mkCharacter1 matt weapon21 filter2 potencies21
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        , { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon11 filter1 potencies11
              , mkCharacter1 matt weapon22 filter2 potencies22
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        , { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon12 filter1 potencies12
              , mkCharacter1 matt weapon21 filter2 potencies21
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        , { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon12 filter1 potencies12
              , mkCharacter1 matt weapon22 filter2 potencies22
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        , { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon13 filter1 potencies13
              , mkCharacter1 matt weapon21 filter2 potencies21
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        , { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon13 filter1 potencies13
              , mkCharacter1 matt weapon22 filter2 potencies22
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        ]

    it "returns no combinations when a match is not found for a required effect" do
      let
        combs = Search.search 3 Set.empty
          [ { filter: filter1
            , matchingWeapons:
                [ { weapon: weapon11, weaponState: defaultWeaponState, potencies: potencies11, matchesFilters: true }
                , { weapon: weapon12, weaponState: defaultWeaponState, potencies: potencies12, matchesFilters: true }
                ]
            }
          , { filter: filter2
            , matchingWeapons:
                [ { weapon: weapon21, weaponState: defaultWeaponState, potencies: potencies21, matchesFilters: true }
                , { weapon: weapon22, weaponState: defaultWeaponState, potencies: potencies22, matchesFilters: true }
                ]
            }
          , { filter: filter3
            , matchingWeapons:
                [
                ]
            }
          ]
      combs `shouldEqualPretty` []

    it "returns no combinations when no effects were selected" do
      Search.search 3 Set.empty [] `shouldEqualPretty` []

    it "discards unmatched weapons" do
      let
        teams = Search.search 3 Set.empty
          [ { filter: filter1
            , matchingWeapons:
                [ { weapon: weapon11, weaponState: defaultWeaponState, potencies: potencies11, matchesFilters: false }
                , { weapon: weapon12, weaponState: defaultWeaponState, potencies: potencies12, matchesFilters: true }
                , { weapon: weapon13, weaponState: defaultWeaponState, potencies: potencies13, matchesFilters: true }
                ]
            }
          , { filter: filter2
            , matchingWeapons:
                [ { weapon: weapon21, weaponState: defaultWeaponState, potencies: potencies21, matchesFilters: true }
                , { weapon: weapon22, weaponState: defaultWeaponState, potencies: potencies22, matchesFilters: true }
                ]
            }
          , { filter: filter3
            , matchingWeapons:
                [ { weapon: weapon31, weaponState: defaultWeaponState, potencies: potencies31, matchesFilters: true }
                ]
            }
          ]

      setAsArray teams `shouldEqualPretty` setAsArray
        [ { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon12 filter1 potencies12
              , mkCharacter1 matt weapon21 filter2 potencies21
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        , { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon12 filter1 potencies12
              , mkCharacter1 matt weapon22 filter2 potencies22
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        , { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon13 filter1 potencies13
              , mkCharacter1 matt weapon21 filter2 potencies21
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        , { characters: Map.fromFoldable
              [ mkCharacter1 aerith weapon13 filter1 potencies13
              , mkCharacter1 matt weapon22 filter2 potencies22
              , mkCharacter1 lucia weapon31 filter3 potencies31
              ]
          }
        ]

assignWeaponSpec :: Spec Unit
assignWeaponSpec = do
  describe "assign weapon" do
    let tifaWeapon1 = mkWeapon (nes @"Tifa 1") tifa
    let tifaWeapon2 = mkWeapon (nes @"Tifa 2") tifa
    let tifaWeapon3 = mkWeapon (nes @"Tifa 3") tifa

    let vincentWeapon1 = mkWeapon (nes @"Vincent 1") vincent

    let redWeapon1 = mkWeapon (nes @"Red XIII 1") red

    let potencies1 = Just { base: Low, max: Low }
    let potencies2 = Just { base: Low, max: Mid }
    let potencies3 = Just { base: Low, max: High }

    it "creates character with 1st weapon in the main hand" do
      Search.assignWeapon 3
        { filter: filter1, potencies: potencies1 }
        tifaWeapon1
        defaultWeaponState
        emptyTeam
        `shouldEqualPretty` Just
          { characters: Map.fromFoldable
              [ mkCharacter1 tifa tifaWeapon1 filter1 potencies1

              ]
          }

    it "assigns 2nd weapon to off hand" do
      ( emptyTeam
          # Search.assignWeapon 3 { filter: filter1, potencies: potencies1 } tifaWeapon1 defaultWeaponState
          >>= Search.assignWeapon 3 { filter: filter2, potencies: potencies2 } tifaWeapon2 defaultWeaponState
      )
        `shouldEqualPretty` Just
          { characters: Map.fromFoldable
              [ mkCharacter2 tifa
                  tifaWeapon1
                  filter1
                  potencies1
                  tifaWeapon2
                  filter2
                  potencies2

              ]
          }

    it "handles main hand weapon matching on 2 or more effects" do
      ( emptyTeam
          # Search.assignWeapon 3 { filter: filter1, potencies: potencies1 } tifaWeapon1 defaultWeaponState
          >>= Search.assignWeapon 3 { filter: filter2, potencies: potencies2 } tifaWeapon1 defaultWeaponState
      )
        `shouldEqualPretty` Just
          { characters: Map.fromFoldable
              [ Tuple "tifa"
                  { name: tifa
                  , mainHand: Just
                      { weaponData: tifaWeapon1
                      , weaponState: defaultWeaponState
                      , matchedFilters:
                          [ { filter: filter2, potencies: potencies2 }
                          , { filter: filter1, potencies: potencies1 }
                          ]
                      }
                  , offHand: Nothing
                  }
              ]
          }

    it "handles off hand weapon matching on 2 or more effects" do
      ( emptyTeam
          # Search.assignWeapon 3 { filter: filter1, potencies: potencies1 } tifaWeapon1 defaultWeaponState
          >>= Search.assignWeapon 3 { filter: filter2, potencies: potencies2 } tifaWeapon2 defaultWeaponState
          >>= Search.assignWeapon 3 { filter: filter3, potencies: potencies3 } tifaWeapon2 defaultWeaponState
      )
        `shouldEqualPretty` Just
          { characters: Map.fromFoldable
              [ Tuple "tifa"
                  { name: tifa
                  , mainHand: Just
                      { weaponData: tifaWeapon1
                      , weaponState: defaultWeaponState
                      , matchedFilters:
                          [ { filter: filter1, potencies: potencies1 }
                          ]
                      }
                  , offHand: Just
                      { weaponData: tifaWeapon2
                      , weaponState: defaultWeaponState
                      , matchedFilters:
                          [ { filter: filter3, potencies: potencies3 }
                          , { filter: filter2, potencies: potencies2 }
                          ]
                      }
                  }
              ]
          }

    it "fails if more than 2 weapons were selected for the same character" do
      ( emptyTeam
          # Search.assignWeapon 3 { filter: filter1, potencies: potencies1 } tifaWeapon1 defaultWeaponState
          >>= Search.assignWeapon 3 { filter: filter2, potencies: potencies2 } tifaWeapon2 defaultWeaponState
          >>= Search.assignWeapon 3 { filter: filter3, potencies: potencies3 } tifaWeapon3 defaultWeaponState
      )
        `shouldEqual` Nothing

    it "fails if character count exceeds the selected maximum" do
      ( emptyTeam
          # Search.assignWeapon 2 { filter: filter1, potencies: potencies1 } tifaWeapon1 defaultWeaponState
          >>= Search.assignWeapon 2 { filter: filter2, potencies: potencies2 } vincentWeapon1 defaultWeaponState
          >>= Search.assignWeapon 2 { filter: filter3, potencies: potencies3 } redWeapon1 defaultWeaponState
      )
        `shouldEqual` Nothing

searchExamplesSpec :: Spec Unit
searchExamplesSpec = do
  describe "search examples" do
    it "example 1" do
      dbState <- T.loadTestDbState
      let
        filters =
          [ { effectType: FilterHeal, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterPatkUp, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterPatkDown, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
          ]
        maxCharacterCount = 1
        excludeChars = Set.empty
        mustHaveChars = Set.empty
        results =
          Search.applyFilters filters dbState
            # Search.search maxCharacterCount excludeChars
            # Search.filterMustHaveChars mustHaveChars
            # Search.filterDuplicates
      T.goldenTest "snaps/search-example-1.snap" $ teamSummary <$> results
    it "example 2" do
      dbState <- T.loadTestDbState
      let
        filters =
          [ { effectType: FilterHeal, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterMatkUp, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterMdefDown, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterFireResistDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterMdefUp, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
          ]
        maxCharacterCount = 2
        excludeChars = Set.empty
        mustHaveChars = Set.empty
        results =
          Search.applyFilters filters dbState
            # Search.search maxCharacterCount excludeChars
            # Search.filterMustHaveChars mustHaveChars
            # Search.filterDuplicates
      T.goldenTest "snaps/search-example-2.snap" $ teamSummary <$> results
    it "example 3" do
      dbState <- T.loadTestDbState
      let
        filters =
          [ { effectType: FilterPdefDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterPatkUp, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterHeal, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterWaterResistDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterPatkDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
          , { effectType: FilterMatkDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
          ]
        maxCharacterCount = 2
        excludeChars = Set.empty
        mustHaveChars = Set.empty
        results =
          Search.applyFilters filters dbState
            # Search.search maxCharacterCount excludeChars
            # Search.filterMustHaveChars mustHaveChars
            # Search.filterDuplicates
      T.goldenTest "snaps/search-example-3.snap" $ teamSummary <$> results
  where
  teamSummary :: AssignmentResult -> _
  teamSummary team =
    team.characters
      # Map.values
      # Arr.fromFoldable
      <#>
        ( \character ->
            do
              let
                weapons = Arr.sort $ Arr.catMaybes
                  [ character.mainHand <#> _.weaponData.weapon.name
                  , character.offHand <#> _.weaponData.weapon.name
                  ]

              { character: character.name
              , weapons
              }
        )
      # Arr.sort

findMatchingWeaponsSpec :: Spec Unit
findMatchingWeaponsSpec = do
  describe "find matching weapons" do
    it "returns potencies according to owned OB level / selected range" do
      dbState <- T.loadTestDbState

      let
        { matchingWeapons } = Search.findMatchingWeapons { effectType: FilterPatkUp, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low } dbState
      weapon <- findWeapon matchingWeapons
      weapon.potencies `shouldEqualPretty` Just { base: High, max: High }

      let
        { matchingWeapons } = Search.findMatchingWeapons { effectType: FilterPatkUp, range: FilterSelfOrSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low } dbState
      weapon <- findWeapon matchingWeapons
      weapon.potencies `shouldEqualPretty` Just { base: High, max: High }

      { matchingWeapons } <-
        dbState
          # setOwnedLevel (ObRange { from: FromOb0, to: ToOb5 })
          <#> Search.findMatchingWeapons { effectType: FilterPatkUp, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
      weapon <- findWeapon matchingWeapons
      weapon.potencies `shouldEqualPretty` Just { base: Mid, max: High }

      { matchingWeapons } <-
        dbState
          # setOwnedLevel (ObRange { from: FromOb0, to: ToOb5 })
          <#> Search.findMatchingWeapons { effectType: FilterPatkUp, range: FilterSelfOrSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
      weapon <- findWeapon matchingWeapons
      weapon.potencies `shouldEqualPretty` Just { base: Mid, max: High }
  where
  findWeapon :: Array FilterResultWeapon -> Aff FilterResultWeapon
  findWeapon matchingWeapons =
    matchingWeapons
      # Arr.find (\w -> w.weapon.weapon.name == WeaponName (nes @"Arctic Star"))
      # maybe (throwError $ error "Failed to find weapon") pure

  setOwnedLevel :: ObRange -> DbState -> Aff DbState
  setOwnedLevel obRange dbState = do
    let
      weapons = Map.update
        (\w -> Just $ w { ownedOb = Just obRange })
        (WeaponName (nes @"Arctic Star"))
        dbState.userState.weapons

    pure $ dbState { userState { weapons = weapons } }

mkWeapon :: NonEmptyString -> CharacterName -> WeaponData
mkWeapon id character =
  { weapon:
      { name: WeaponName id
      , character
      , source: nes @"Gacha"
      , image: nes @" "
      , atbCost: 3
      , ob0: { description: nes @" ", effects: [] }
      , ob1: { description: nes @" ", effects: [] }
      , ob6: { description: nes @" ", effects: [] }
      , ob10: { description: nes @" ", effects: [] }
      , cureAllAbility: true
      }
  , distinctObs: NAR.singleton (ObRange { from: FromOb0, to: ToOb5 })
      # NAR.cons (ObRange { from: FromOb6, to: ToOb10 })
  }

filter1 :: Filter
filter1 =
  { effectType: FilterHeal
  , range: FilterAll
  , minBasePotency: Low
  , minMaxPotency: Low
  }

filter2 :: Filter
filter2 =
  { effectType: FilterPatkDown
  , range: FilterSingleTargetOrAll
  , minBasePotency: Low
  , minMaxPotency: Low
  }

filter3 :: Filter
filter3 =
  { effectType: FilterProvoke
  , range: FilterSelfOrSingleTargetOrAll
  , minBasePotency: Low
  , minMaxPotency: Low
  }

mkCharacter1 :: CharacterName -> WeaponData -> Filter -> Maybe Potencies -> Tuple String Character
mkCharacter1 characterName weaponData filter potencies =
  Tuple (NES.toString $ unwrap characterName)
    { name: characterName
    , mainHand: Just
        { weaponData
        , weaponState: defaultWeaponState
        , matchedFilters: [ { filter, potencies } ]
        }
    , offHand: Nothing
    }

mkCharacter2
  :: CharacterName
  -> WeaponData
  -> Filter
  -> Maybe Potencies
  -> WeaponData
  -> Filter
  -> Maybe Potencies
  -> Tuple String Character
mkCharacter2 characterName weapon1 filter1 potencies1 weapon2 filter2 potencies2 =
  Tuple (NES.toString $ unwrap characterName)
    { name: characterName
    , mainHand: Just
        { weaponData: weapon1
        , weaponState: defaultWeaponState
        , matchedFilters: [ { filter: filter1, potencies: potencies1 } ]
        }
    , offHand: Just
        { weaponData: weapon2
        , weaponState: defaultWeaponState
        , matchedFilters: [ { filter: filter2, potencies: potencies2 } ]
        }
    }

defaultWeaponState :: UserStateWeapon
defaultWeaponState =
  { ownedOb: Just (ObRange { from: FromOb6, to: ToOb10 })
  , ignored: false
  }
