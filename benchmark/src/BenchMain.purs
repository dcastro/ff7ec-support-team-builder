module BenchMain where

import Prelude

import Benchmarks.CartesianProduct as CartesianProduct
import Benchmarks.Search as Search
import Benchotron.UI.Console (runSuite)
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)

main :: Effect Unit
main = do
  runAff_ (either throwException (const (pure unit))) do
    search <- Search.benchSearch
    liftEffect $ runSuite
      [ CartesianProduct.benchCartesianProduct
      , CartesianProduct.benchCartesianProductAndFilter
      , search
      ]
