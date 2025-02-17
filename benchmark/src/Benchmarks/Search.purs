module Benchmarks.Search where

import Core.Database.VLatest
import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Control.Monad.Error.Class (throwError)
import Core.Database as Db
import Core.Weapons.Parser (parseWeapons)
import Core.Weapons.Search (AssignmentResult, Filter, FilterRange(..))
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
        ]
    }

input :: Array Filter
input =
  [ { effectType: FilterPdefDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
  , { effectType: FilterPatkUp, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
  , { effectType: FilterHeal, range: FilterAll, minBasePotency: Low, minMaxPotency: Low }
  , { effectType: FilterWaterResistDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
  , { effectType: FilterPatkDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
  , { effectType: FilterMatkDown, range: FilterSingleTargetOrAll, minBasePotency: Low, minMaxPotency: Low }
  ]

search1 :: Db -> Array Filter -> Array AssignmentResult
search1 db filters = do
  let
    maxCharacterCount = 2
    mustHaveChars = Set.empty
  Search.applyFilters filters db
    # Search.search maxCharacterCount
    # Search.filterMustHaveChars mustHaveChars
    # Search.filterDuplicates

loadTestDb :: Aff Db
loadTestDb = do
  sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
  sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
    Right res -> pure res.values
    Left errs ->
      throwError $ error
        $ "Failed to read `resources/weapons.json`: \n"
            <> Utils.renderJsonErr errs
  let { weapons, errors: _ } = parseWeapons sourceWeapons

  Db.createDb weapons Map.empty
