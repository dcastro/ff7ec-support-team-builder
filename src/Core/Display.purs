module Core.Display where

import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES

class Display a where
  display :: a -> String

instance Display NonEmptyString where
  display = NES.toString
