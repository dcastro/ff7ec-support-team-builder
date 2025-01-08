module Main where

import Prelude

import App.Button as Button
import Core.Weapons.Parser as P
import Data.Array as Array
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.String.NonEmpty as NES
import Effect (Effect)
import Effect.Class.Console as Console
import Google.SheetsApi as SheetsApi
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    grid <- SheetsApi.getSheet "Weapons!A:Z"
    let
      rr = P.parseWeapons grid.result.values
    case rr of
      Left err -> Console.log err
      Right weapons -> do
        let
          weapon = weapons # Array.find \w -> NES.toString (unwrap w.name) == "Kamura Wand"
        Console.logShow weapon
        pure unit
    body <- HA.awaitBody
    runUI Button.component unit body
