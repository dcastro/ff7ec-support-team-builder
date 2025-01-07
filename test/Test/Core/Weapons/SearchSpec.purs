module Test.Core.Weapons.SearchSpec where

import Prelude
import Test.Spec
import Core.Weapons.Search (FilterEffectType(..), FilterRange(..))
import Core.Weapons.Search as Search
import Data.Maybe (Maybe(..))
import Test.Utils (nes, shouldEqualPretty)

spec :: Spec Unit
spec =
  describe "search" do
    describe "combinations" do
      it "finds all possible combinations" do
        let
          mkWeapon id =
            { name: id
            , character: nes @" "
            , image: nes @" "
            , ob0: { description: nes @" ", effects: [] }
            , ob1: { description: nes @" ", effects: [] }
            , ob6: { description: nes @" ", effects: [] }
            , ob10: { description: nes @" ", effects: [] }
            , cureAllAbility: true
            }
          filter1 =
            { effectType: FilterHeal
            , range: FilterAll
            }
          weapon11 = mkWeapon $ nes @"11"
          weapon12 = mkWeapon $ nes @"12"
          weapon13 = mkWeapon $ nes @"13"

          filter2 =
            { effectType: FilterPatkDown
            , range: FilterSingleTargetOrAll
            }
          weapon21 = mkWeapon $ nes @"21"
          weapon22 = mkWeapon $ nes @"22"

          filter3 =
            { effectType: FilterProvoke
            , range: FilterSelfOrSingleTargetOrAll
            }
          weapon31 = mkWeapon $ nes @"31"

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
