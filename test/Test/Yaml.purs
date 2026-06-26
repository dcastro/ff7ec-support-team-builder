module Test.Yaml where

import Prelude

import Foreign (Foreign)
import Yoga.JSON (class WriteForeign, write)

foreign import writeYAMLImpl :: Foreign -> String

-- | Serialize a value to a YAML string, using its `WriteForeign` instance.
writePrettyYAML :: forall a. WriteForeign a => a -> String
writePrettyYAML = write >>> writeYAMLImpl
