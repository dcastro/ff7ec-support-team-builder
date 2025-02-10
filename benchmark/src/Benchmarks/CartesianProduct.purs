module Benchmarks.CartesianProduct where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Data.Array as Arr
import Data.Int (odd)
import Data.Maybe (Maybe(..))

benchCartesianProduct :: Benchmark
benchCartesianProduct = mkBenchmark
  { slug: "cart"
  , title: "Cartesian Product"
  , sizes: [ 8, 10, 12 ]
  , sizeInterpretation: "Number of elements in the inner arrays"
  , inputsPerSize: 1
  , gen: \n -> pure $ genInput n
  , functions:
      -- All 4 are pretty much the same
      [ benchFn "cartesian1: foldl + snoc" cartesian1
      , benchFn "cartesian2: foldl + cons" cartesian2
      , benchFn "cartesian3: foldr + cons" cartesian3
      , benchFn "cartesian4: javascript" cartesian4
      ]
  }

genInput :: Int -> Array (Array Int)
genInput n =
  Arr.range 0 5 <#> \_ -> do
    Arr.range 0 n <#> \j -> j

cartesian1 :: Array (Array Int) -> Array (Array Int)
cartesian1 =
  Arr.foldl
    ( \b arr -> ado
        x <- arr
        xs <- b
        in Arr.snoc xs x
    )
    [ [] ]

cartesian2 :: Array (Array Int) -> Array (Array Int)
cartesian2 =
  Arr.foldl
    ( \b arr -> ado
        x <- arr
        xs <- b
        in Arr.cons x xs
    )
    [ [] ]

cartesian3 :: Array (Array Int) -> Array (Array Int)
cartesian3 =
  Arr.foldr
    ( \arr b -> ado
        x <- arr
        xs <- b
        in Arr.cons x xs
    )
    [ [] ]

foreign import cartesian4 :: Array (Array Int) -> Array (Array Int)

benchCartesianProductAndFilter :: Benchmark
benchCartesianProductAndFilter = mkBenchmark
  { slug: "cart_filter"
  , title: "Cartesian Product + Filter"
  , sizes: [ 8, 10, 12 ]
  , sizeInterpretation: "Number of elements in the inner arrays"
  , inputsPerSize: 1
  , gen: \n -> pure $ genInput n
  , functions:
      -- 2 and 3 are pretty much the same (though 2 is not stack-safe for large enough arrays)
      -- 1 is much slower
      [ benchFn "cartesianFilter1: applicative do + catMaybes" cartesianFilter1
      , benchFn "cartesianFilter2: monadic do" cartesianFilter2
      , benchFn "cartesianFilter3: javascript" cartesianFilter3
      ]
  }

cartesianFilter1 :: Array (Array Int) -> Array (Array Int)
cartesianFilter1 =
  Arr.foldl
    ( \b arr -> ado
        x <- arr
        xs <- b
        in
          case xs of
            Nothing -> Nothing
            Just xs ->
              if odd x then Nothing
              else Just $ Arr.cons x xs
    )
    [ Just [] ]
    >>> Arr.catMaybes

-- NOTE: Monadic bind for arrays is not stack-safe
-- https://discourse.purescript.org/t/stack-safe-cartesian-product/4841
cartesianFilter2 :: Array (Array Int) -> Array (Array Int)
cartesianFilter2 =
  Arr.foldl
    ( \b arr -> do
        x <- arr
        xs <- b
        if odd x then []
        else [ Arr.cons x xs ]
    )
    [ [] ]

foreign import cartesianFilter3 :: Array (Array Int) -> Array (Array Int)
