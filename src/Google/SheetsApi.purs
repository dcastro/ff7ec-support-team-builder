module Google.SheetsApi where

import Prelude
import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

{-
  FFI resources:
  * https://github.com/purescript/documentation/blob/master/guides/FFI-Tips.md
  * https://book.purescript.org/chapter10.html
-}
foreign import _getSheet :: String -> Effect (Promise (Response GetSheetResult))

getSheet :: String -> Aff (Response GetSheetResult)
getSheet range = do
  toAffE $ _getSheet range

type Response r
  = { status :: Int
    , result :: r
    }

type GetSheetResult
  = { values :: Array (Array String)
    }
