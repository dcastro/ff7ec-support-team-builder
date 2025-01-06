module Core.WebStorage where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as WS

setItem :: String -> String -> Effect Unit
setItem key value = do
  w <- window
  s <- localStorage w
  WS.setItem key value s

getItem :: String -> Effect (Maybe String)
getItem key = do
  w <- window
  s <- localStorage w
  WS.getItem key s
