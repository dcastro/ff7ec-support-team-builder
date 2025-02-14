module Test.Core.Weapons.SearchSpec (spec) where

import Core.Database.VLatest
import Prelude
import Test.Spec
import Test.Utils

import Core.Weapons.Search (AssignmentResult, Character, Filter, FilterRange(..), emptyTeam)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Array.NonEmpty as NAR
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Test.Spec.Assertions (shouldEqual)
import Test.Utils as T

spec :: Spec Unit
spec =
  describe "search" do
    combinationsSpec
    assignWeaponSpec
    searchExamplesSpec

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

      potencies11 = Just $ mkAllPotencies { base: Low, max: Low }
      potencies12 = Just $ mkAllPotencies { base: Low, max: Mid }
      potencies13 = Just $ mkAllPotencies { base: Low, max: High }
      potencies21 = Just $ mkAllPotencies { base: Mid, max: Mid }
      potencies22 = Just $ mkAllPotencies { base: Mid, max: High }
      potencies31 = Just $ mkAllPotencies { base: High, max: High }

    it "finds all possible combinations" do
      let
        teams = Search.search 3
          [ { filter: filter1
            , matchingWeapons:
                [ { weapon: weapon11, allPotencies: potencies11 }
                , { weapon: weapon12, allPotencies: potencies12 }
                , { weapon: weapon13, allPotencies: potencies13 }
                ]
            }
          , { filter: filter2
            , matchingWeapons:
                [ { weapon: weapon21, allPotencies: potencies21 }
                , { weapon: weapon22, allPotencies: potencies22 }
                ]
            }
          , { filter: filter3
            , matchingWeapons:
                [ { weapon: weapon31, allPotencies: potencies31 }
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
        combs = Search.search 3
          [ { filter: filter1
            , matchingWeapons:
                [ { weapon: weapon11, allPotencies: potencies11 }
                , { weapon: weapon12, allPotencies: potencies12 }
                ]
            }
          , { filter: filter2
            , matchingWeapons:
                [ { weapon: weapon21, allPotencies: potencies21 }
                , { weapon: weapon22, allPotencies: potencies22 }
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
      Search.search 3 [] `shouldEqualPretty` []

    it "discards ignored weapons" do
      let
        teams = Search.search 3
          [ { filter: filter1
            , matchingWeapons:
                [ { weapon: weapon11 { ignored = true }, allPotencies: potencies11 }
                , { weapon: weapon12, allPotencies: potencies12 }
                , { weapon: weapon13, allPotencies: potencies13 }
                ]
            }
          , { filter: filter2
            , matchingWeapons:
                [ { weapon: weapon21, allPotencies: potencies21 }
                , { weapon: weapon22, allPotencies: potencies22 }
                ]
            }
          , { filter: filter3
            , matchingWeapons:
                [ { weapon: weapon31, allPotencies: potencies31 }
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

    let potencies1 = Just $ mkAllPotencies { base: Low, max: Low }
    let potencies2 = Just $ mkAllPotencies { base: Low, max: Mid }
    let potencies3 = Just $ mkAllPotencies { base: Low, max: High }

    it "creates character with 1st weapon in the main hand" do
      Search.assignWeapon 3
        { filter: filter1, allPotencies: potencies1 }
        tifaWeapon1
        emptyTeam
        `shouldEqualPretty` Just
          { characters: Map.fromFoldable
              [ mkCharacter1 tifa tifaWeapon1 filter1 potencies1

              ]
          }

    it "assigns 2nd weapon to off hand" do
      ( emptyTeam
          # Search.assignWeapon 3 { filter: filter1, allPotencies: potencies1 } tifaWeapon1
          >>= Search.assignWeapon 3 { filter: filter2, allPotencies: potencies2 } tifaWeapon2
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
          # Search.assignWeapon 3 { filter: filter1, allPotencies: potencies1 } tifaWeapon1
          >>= Search.assignWeapon 3 { filter: filter2, allPotencies: potencies2 } tifaWeapon1
      )
        `shouldEqualPretty` Just
          { characters: Map.fromFoldable
              [ Tuple "tifa"
                  { name: tifa
                  , mainHand: Just
                      { weaponData: tifaWeapon1
                      , matchedFilters:
                          [ { filter: filter2, allPotencies: potencies2 }
                          , { filter: filter1, allPotencies: potencies1 }
                          ]
                      }
                  , offHand: Nothing
                  }
              ]
          }

    it "handles off hand weapon matching on 2 or more effects" do
      ( emptyTeam
          # Search.assignWeapon 3 { filter: filter1, allPotencies: potencies1 } tifaWeapon1
          >>= Search.assignWeapon 3 { filter: filter2, allPotencies: potencies2 } tifaWeapon2
          >>= Search.assignWeapon 3 { filter: filter3, allPotencies: potencies3 } tifaWeapon2
      )
        `shouldEqualPretty` Just
          { characters: Map.fromFoldable
              [ Tuple "tifa"
                  { name: tifa
                  , mainHand: Just
                      { weaponData: tifaWeapon1
                      , matchedFilters:
                          [ { filter: filter1, allPotencies: potencies1 }
                          ]
                      }
                  , offHand: Just
                      { weaponData: tifaWeapon2
                      , matchedFilters:
                          [ { filter: filter3, allPotencies: potencies3 }
                          , { filter: filter2, allPotencies: potencies2 }
                          ]
                      }
                  }
              ]
          }

    it "fails if more than 2 weapons were selected for the same character" do
      ( emptyTeam
          # Search.assignWeapon 3 { filter: filter1, allPotencies: potencies1 } tifaWeapon1
          >>= Search.assignWeapon 3 { filter: filter2, allPotencies: potencies2 } tifaWeapon2
          >>= Search.assignWeapon 3 { filter: filter3, allPotencies: potencies3 } tifaWeapon3
      )
        `shouldEqual` Nothing

    it "fails if character count exceeds the selected maximum" do
      ( emptyTeam
          # Search.assignWeapon 2 { filter: filter1, allPotencies: potencies1 } tifaWeapon1
          >>= Search.assignWeapon 2 { filter: filter2, allPotencies: potencies2 } vincentWeapon1
          >>= Search.assignWeapon 2 { filter: filter3, allPotencies: potencies3 } redWeapon1
      )
        `shouldEqual` Nothing

searchExamplesSpec :: Spec Unit
searchExamplesSpec = do
  describe "search examples" do
    it "example 1" do
      armory <- T.loadTestDb
      let
        filters =
          [ { effectType: FilterHeal, range: FilterAll }
          , { effectType: FilterPatkUp, range: FilterAll }
          , { effectType: FilterPatkDown, range: FilterAll }
          ]
        maxCharacterCount = 1
        mustHaveChars = Set.empty
        results =
          Search.applyFilters filters armory
            # Search.search maxCharacterCount
            # Search.filterMustHaveChars mustHaveChars
            # Search.filterDuplicates
      T.goldenTest "snaps/search-example-1.snap" $ teamSummary <$> results
    it "example 2" do
      armory <- T.loadTestDb
      let
        filters =
          [ { effectType: FilterHeal, range: FilterAll }
          , { effectType: FilterMatkUp, range: FilterAll }
          , { effectType: FilterMdefDown, range: FilterAll }
          , { effectType: FilterFireResistDown, range: FilterSingleTargetOrAll }
          , { effectType: FilterMdefUp, range: FilterAll }
          ]
        maxCharacterCount = 2
        mustHaveChars = Set.empty
        results =
          Search.applyFilters filters armory
            # Search.search maxCharacterCount
            # Search.filterMustHaveChars mustHaveChars
            # Search.filterDuplicates
      T.goldenTest "snaps/search-example-2.snap" $ teamSummary <$> results
    it "example 3" do
      armory <- T.loadTestDb
      let
        filters =
          [ { effectType: FilterPdefDown, range: FilterSingleTargetOrAll }
          , { effectType: FilterPatkUp, range: FilterSingleTargetOrAll }
          , { effectType: FilterHeal, range: FilterAll }
          , { effectType: FilterWaterResistDown, range: FilterSingleTargetOrAll }
          , { effectType: FilterPatkDown, range: FilterSingleTargetOrAll }
          , { effectType: FilterMatkDown, range: FilterSingleTargetOrAll }
          ]
        maxCharacterCount = 2
        mustHaveChars = Set.empty
        results =
          Search.applyFilters filters armory
            # Search.search maxCharacterCount
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
  , ignored: false
  , distinctObs: NAR.singleton (ObRange { from: FromOb0, to: Just ToOb5 })
      # NAR.cons (ObRange { from: FromOb6, to: Just ToOb10 })
  , ownedOb: Just (ObRange { from: FromOb6, to: Just ToOb10 })
  }

filter1 :: Filter
filter1 =
  { effectType: FilterHeal
  , range: FilterAll
  }

filter2 :: Filter
filter2 =
  { effectType: FilterPatkDown
  , range: FilterSingleTargetOrAll
  }

filter3 :: Filter
filter3 =
  { effectType: FilterProvoke
  , range: FilterSelfOrSingleTargetOrAll
  }

mkCharacter1 :: CharacterName -> WeaponData -> Filter -> Maybe AllPotencies -> Tuple String Character
mkCharacter1 characterName weaponData filter allPotencies =
  Tuple (NES.toString $ unwrap characterName)
    { name: characterName
    , mainHand: Just
        { weaponData
        , matchedFilters: [ { filter, allPotencies } ]
        }
    , offHand: Nothing
    }

mkCharacter2
  :: CharacterName
  -> WeaponData
  -> Filter
  -> Maybe AllPotencies
  -> WeaponData
  -> Filter
  -> Maybe AllPotencies
  -> Tuple String Character
mkCharacter2 characterName weapon1 filter1 potencies1 weapon2 filter2 potencies2 =
  Tuple (NES.toString $ unwrap characterName)
    { name: characterName
    , mainHand: Just
        { weaponData: weapon1
        , matchedFilters: [ { filter: filter1, allPotencies: potencies1 } ]
        }
    , offHand: Just
        { weaponData: weapon2
        , matchedFilters: [ { filter: filter2, allPotencies: potencies2 } ]
        }
    }

mkAllPotencies :: Potencies -> AllPotencies
mkAllPotencies pots =
  { ob0: pots
  , ob1: pots
  , ob6: pots
  , ob10: pots
  }
