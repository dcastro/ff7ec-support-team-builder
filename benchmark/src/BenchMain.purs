module BenchMain where

import Prelude

import Benchmarks.CartesianProduct as CartesianProduct
import Benchotron.UI.Console (runSuite)
import Effect (Effect)

main :: Effect Unit
main = do
  runSuite
    [ CartesianProduct.benchCartesianProduct
    , CartesianProduct.benchCartesianProductAndFilter
    ]
