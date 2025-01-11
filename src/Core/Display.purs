module Core.Display where

import Prelude
import Core.Weapons.Search (FilterRange(..))

class Display a where
  display :: a -> String

instance Display FilterRange where
  display = case _ of
    FilterAll -> "All"
    FilterSingleTargetOrAll -> "Single Target / All"
    FilterSelfOrSingleTargetOrAll -> "Self / Single Target / All"
