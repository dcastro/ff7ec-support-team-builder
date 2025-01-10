module Main where

import Prelude

import App.Button as Button
import Core.Armory as Armory
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.NonEmpty as NES
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    Armory.init >>= case _ of
      Nothing -> pure unit
      Just armory -> do
        let
          weapon = armory # Array.find \w -> NES.toString (unwrap w.name) == "Kamura Wand"
        Console.logShow weapon
        pure unit
    body <- HA.awaitBody
    runUI Button.component unit body
