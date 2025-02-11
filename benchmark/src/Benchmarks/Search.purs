module Benchmarks.Search where

import Core.Database.VLatest
import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Control.Monad.Error.Class (throwError)
import Core.Armory as Armory
import Core.Weapons.Parser (parseWeapons)
import Core.Weapons.Search (AssignmentResult)
import Core.Weapons.Search as Search
import Data.Either (Either(..))
import Data.Map as Map
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Exception (error)
import Google.SheetsApi (GetSheetResult)
import Node.Encoding as Node
import Node.FS.Aff as Node
import Utils as Utils
import Yoga.JSON as J

benchSearch :: Aff Benchmark
benchSearch = do
  db <- loadTestDb
  pure $ mkBenchmark
    { slug: "search"
    , title: "Search"
    , sizes: [ 1, 2 ]
    , sizeInterpretation: "--"
    , inputsPerSize: 1
    , gen: \_ -> pure input
    , functions:
        [ benchFn "search1" $ search1 db
        , benchFn "search2" $ search2 db
        ]
    }

input :: Array Filter
input =
  [ { effectType: FilterPdefDown, range: FilterSingleTargetOrAll }
  , { effectType: FilterPatkUp, range: FilterSingleTargetOrAll }
  , { effectType: FilterHeal, range: FilterAll }
  , { effectType: FilterWaterResistDown, range: FilterSingleTargetOrAll }
  , { effectType: FilterPatkDown, range: FilterSingleTargetOrAll }
  , { effectType: FilterMatkDown, range: FilterSingleTargetOrAll }
  ]

search1 :: Armory -> Array Filter -> Array AssignmentResult
search1 armory filters = do
  let
    maxCharacterCount = 2
    mustHaveChars = Set.empty
  Search.search maxCharacterCount filters armory
    # Search.filterMustHaveChars mustHaveChars
    # Search.filterDuplicates

search2 :: Armory -> Array Filter -> Array AssignmentResult
search2 armory filters = do
  let
    maxCharacterCount = 2
    mustHaveChars = Set.empty
  Search.search2 maxCharacterCount filters armory
    # Search.filterMustHaveChars mustHaveChars
    # Search.filterDuplicates

loadTestDb :: Aff Armory
loadTestDb = do
  sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
  sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
    Right res -> pure res.values
    Left errs ->
      throwError $ error
        $ "Failed to read `resources/weapons.json`: \n"
            <> Utils.renderJsonErr errs
  let { weapons, errors: _ } = parseWeapons sourceWeapons

  Armory.createArmory weapons Map.empty
