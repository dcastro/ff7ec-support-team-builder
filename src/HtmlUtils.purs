module HtmlUtils where

import Prelude

import Core.Database.Types (Weapon)
import Core.Display (display)
import Data.String.Utils as String
import Halogen (AttrName(..), ClassName(..))
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

classes' :: forall r i. String -> IProp (class :: String | r) i
classes' str =
  HP.classes $ ClassName <$> String.words str

tooltip :: forall r i. String -> IProp r i
tooltip text = HH.attr (AttrName "data-tooltip") text

mkTooltipForWeapon :: Weapon -> String
mkTooltipForWeapon weapon =
  "Click the weapon for more details.\n\nOB10:\n"
    <> display weapon.ob10.description

displayIf :: forall w i. Boolean -> HTML w i -> HTML w i
displayIf cond html =
  if cond then html else HH.text ""
