module HtmlUtils where

import Prelude

import Data.String.Utils as String
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP

classes' :: forall r i. String -> IProp (class :: String | r) i
classes' str =
  HP.classes $ ClassName <$> String.words str
