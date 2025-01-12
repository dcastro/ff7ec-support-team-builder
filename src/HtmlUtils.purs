module HtmlUtils where

import Prelude

import Data.String.Utils as String
import Halogen (AttrName(..), ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

classes' :: forall r i. String -> IProp (class :: String | r) i
classes' str =
  HP.classes $ ClassName <$> String.words str

tooltip :: forall r i. String -> IProp r i
tooltip text = HH.attr (AttrName "data-tooltip") text
