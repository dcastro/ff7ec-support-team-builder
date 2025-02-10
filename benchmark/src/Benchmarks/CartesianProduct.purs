module Benchmarks.CartesianProduct where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Data.Array as Arr

benchCartesianProduct :: Benchmark
benchCartesianProduct = mkBenchmark
  { slug: "cart"
  , title: "Cartesian Product"
  , sizes: [ 8, 10, 12 ]
  , sizeInterpretation: "Number of elements in the inner arrays"
  , inputsPerSize: 1
  , gen: \n -> pure $ genInput n
  , functions:
      [ benchFn "cartesian1: foldl + snoc" cartesian1
      , benchFn "cartesian2: foldl + cons" cartesian2
      , benchFn "cartesian3: foldr + cons" cartesian3
      , benchFn "cartesian4: native" cartesian4
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
