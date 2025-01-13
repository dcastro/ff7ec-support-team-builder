module Test.Core.Weapons.SearchSpec (spec) where

import Prelude
import Test.Spec

import Core.Armory (Filter, FilterEffectType(..), FilterRange(..), ArmoryWeapon)
import Core.Weapons.Search as Search
import Core.Weapons.Types (CharacterName(..), WeaponName(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Nullable as Null
import Data.String.NonEmpty (NonEmptyString)
import Data.Tuple (Tuple(..))
import Test.Utils (nes, shouldEqualPretty)

spec :: Spec Unit
spec =
  describe "search" do
    combinationsSpec
    assignWeaponsToCharactersSpec

combinationsSpec :: Spec Unit
combinationsSpec = do
  describe "combinations" do
    let
      weapon11 = mkWeapon (nes @"11") (nes @" ")
      weapon12 = mkWeapon (nes @"12") (nes @" ")
      weapon13 = mkWeapon (nes @"13") (nes @" ")
      weapon21 = mkWeapon (nes @"21") (nes @" ")
      weapon22 = mkWeapon (nes @"22") (nes @" ")
      weapon31 = mkWeapon (nes @"31") (nes @" ")

    it "finds all possible combinations" do
      let
        combs = Search.combinations
          [ { filter: filter1
            , required: true
            , matchingWeapons:
                [ weapon11
                , weapon12
                , weapon13
                ]
            }
          , { filter: filter2
            , required: true
            , matchingWeapons:
                [ weapon21
                , weapon22
                ]
            }
          , { filter: filter3
            , required: true
            , matchingWeapons:
                [ weapon31
                ]
            }
          ]

      combs `shouldEqualPretty`
        [ [ { filter: filter1, weapon: Just weapon11 }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Just weapon11 }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Just weapon12 }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Just weapon12 }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Just weapon13 }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Just weapon13 }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        ]
    it "returns no combinations when a match is not found for a required effect" do
      let
        combs = Search.combinations
          [ { filter: filter1
            , required: true
            , matchingWeapons:
                [ weapon11
                , weapon12
                ]
            }
          , { filter: filter2
            , required: true
            , matchingWeapons:
                [ weapon21
                , weapon22
                ]
            }
          , { filter: filter3
            , required: true
            , matchingWeapons:
                [
                ]
            }
          ]
      combs `shouldEqualPretty` []
    it "returns partial combinations when a match is not found for an optional effect" do
      let
        combs = Search.combinations
          [ { filter: filter1
            , required: true
            , matchingWeapons:
                [ weapon11
                , weapon12
                , weapon13
                ]
            }
          , { filter: filter2
            , required: true
            , matchingWeapons:
                [ weapon21
                , weapon22
                ]
            }
          , { filter: filter3
            , required: false
            , matchingWeapons:
                [
                ]
            }
          ]
      combs `shouldEqualPretty`
        [ [ { filter: filter1, weapon: Just weapon11 }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Nothing }
          ]
        , [ { filter: filter1, weapon: Just weapon11 }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Nothing }
          ]
        , [ { filter: filter1, weapon: Just weapon12 }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Nothing }
          ]
        , [ { filter: filter1, weapon: Just weapon12 }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Nothing }
          ]
        , [ { filter: filter1, weapon: Just weapon13 }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Nothing }
          ]
        , [ { filter: filter1, weapon: Just weapon13 }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Nothing }
          ]
        ]
    it "discards ignored weapons" do
      let
        combs = Search.combinations
          [ { filter: filter1
            , required: true
            , matchingWeapons:
                [ weapon11 { ignored = true }
                , weapon12
                , weapon13
                ]
            }
          , { filter: filter2
            , required: true
            , matchingWeapons:
                [ weapon21
                , weapon22
                ]
            }
          , { filter: filter3
            , required: true
            , matchingWeapons:
                [ weapon31
                ]
            }
          ]

      combs `shouldEqualPretty`
        [ [ { filter: filter1, weapon: Just weapon12 }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Just weapon12 }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Just weapon13 }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Just weapon13 }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        ]
    it "discards ignored weapons in optional filters" do
      let
        combs = Search.combinations
          [ { filter: filter1
            , required: false
            , matchingWeapons:
                [ weapon11 { ignored = true }
                ]
            }
          , { filter: filter2
            , required: true
            , matchingWeapons:
                [ weapon21
                , weapon22
                ]
            }
          , { filter: filter3
            , required: true
            , matchingWeapons:
                [ weapon31
                ]
            }
          ]

      combs `shouldEqualPretty`
        [ [ { filter: filter1, weapon: Nothing }
          , { filter: filter2, weapon: Just weapon21 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        , [ { filter: filter1, weapon: Nothing }
          , { filter: filter2, weapon: Just weapon22 }
          , { filter: filter3, weapon: Just weapon31 }
          ]
        ]

assignWeaponsToCharactersSpec :: Spec Unit
assignWeaponsToCharactersSpec = do
  describe "assignWeaponsToCharacters" do
    let tifaWeapon1 = mkWeapon (nes @"Tifa 1") (nes @"Tifa")
    let tifaWeapon2 = mkWeapon (nes @"Tifa 2") (nes @"Tifa")
    let tifaWeapon3 = mkWeapon (nes @"Tifa 3") (nes @"Tifa")

    let vincentWeapon1 = mkWeapon (nes @"Vincent 1") (nes @"Vincent")
    let vincentWeapon2 = mkWeapon (nes @"Vincent 2") (nes @"Vincent")

    let redWeapon1 = mkWeapon (nes @"Red XIII 1") (nes @"Red XIII")

    it "assigns weapons correctly" do
      let
        combination =
          [ { filter: filter1, weapon: Just tifaWeapon1 }
          , { filter: filter2, weapon: Just tifaWeapon2 }
          , { filter: filter3, weapon: Just vincentWeapon1 }
          , { filter: filter4, weapon: Just vincentWeapon2 }
          ]

      Search.assignWeaponsToCharacters 3 combination `shouldEqualPretty`
        Just
          { characters:
              ( Map.fromFoldable
                  [ Tuple "Tifa"
                      { name: CharacterName $ nes @"Tifa"
                      , mainHand:
                          { weapon: tifaWeapon1
                          , matchedFilters: [ filter1 ]
                          }
                      , offHand: Just
                          { weapon: tifaWeapon2
                          , matchedFilters: [ filter2 ]
                          }
                      }
                  , Tuple "Vincent"
                      { name: CharacterName $ nes @"Vincent"
                      , mainHand:
                          { weapon: vincentWeapon1
                          , matchedFilters: [ filter3 ]
                          }
                      , offHand: Just
                          { weapon: vincentWeapon2
                          , matchedFilters: [ filter4 ]
                          }
                      }
                  ]
              )
          , missedFilters: []
          }

    it "handles a weapon matching on 2 or more effects" do
      let
        combination =
          [ { filter: filter1, weapon: Just tifaWeapon1 }
          , { filter: filter2, weapon: Just tifaWeapon1 }
          , { filter: filter3, weapon: Just tifaWeapon2 }
          , { filter: filter4, weapon: Just tifaWeapon2 }
          , { filter: filter5, weapon: Just vincentWeapon1 }
          ]

      Search.assignWeaponsToCharacters 3 combination `shouldEqualPretty`
        Just
          { characters:
              ( Map.fromFoldable
                  [ Tuple "Tifa"
                      { name: CharacterName $ nes @"Tifa"
                      , mainHand:
                          { weapon: tifaWeapon1
                          , matchedFilters: [ filter2, filter1 ]
                          }
                      , offHand: Just
                          { weapon: tifaWeapon2
                          , matchedFilters: [ filter4, filter3 ]
                          }
                      }
                  , Tuple "Vincent"
                      { name: CharacterName $ nes @"Vincent"
                      , mainHand:
                          { weapon: vincentWeapon1
                          , matchedFilters: [ filter5 ]
                          }
                      , offHand: Nothing
                      }
                  ]
              )
          , missedFilters: []
          }

    it "fails if more than 2 weapons were selected for the same character" do
      let
        combination =
          [ { filter: filter1, weapon: Just tifaWeapon1 }
          , { filter: filter2, weapon: Just tifaWeapon2 }
          , { filter: filter3, weapon: Just tifaWeapon3 }
          ]

      Null.toNullable (Search.assignWeaponsToCharacters 3 combination) `shouldEqualPretty` null

    it "fails if more than 2 characters were selected" do
      let
        combination =
          [ { filter: filter1, weapon: Just tifaWeapon1 }
          , { filter: filter2, weapon: Just vincentWeapon1 }
          , { filter: filter3, weapon: Just redWeapon1 }
          ]

      Null.toNullable (Search.assignWeaponsToCharacters 2 combination) `shouldEqualPretty` null

mkWeapon :: NonEmptyString -> NonEmptyString -> ArmoryWeapon
mkWeapon id character =
  { name: WeaponName id
  , character: CharacterName character
  , image: nes @" "
  , ob0: { description: nes @" ", effects: [] }
  , ob1: { description: nes @" ", effects: [] }
  , ob6: { description: nes @" ", effects: [] }
  , ob10: { description: nes @" ", effects: [] }
  , cureAllAbility: true
  , ignored: false
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

filter4 :: Filter
filter4 =
  { effectType: FilterVeil
  , range: FilterSelfOrSingleTargetOrAll
  }

filter5 :: Filter
filter5 =
  { effectType: FilterIceResistDown
  , range: FilterAll
  }
