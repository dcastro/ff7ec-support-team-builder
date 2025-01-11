module App.EffectSelector where

import Prelude

import Core.Armory (Armory)
import Core.Armory as Armory
import Core.Display (display)
import Core.Weapons.Search (FilterEffectType(..), FilterRange(..))
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Bounded.Generic (genericBottom)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import HtmlUtils (classes')
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Utils (unsafeFromJust)
import Web.Event.Event (Event)

type State =
  { armory ::
      Armory
  , selectedEffectType :: Maybe FilterEffectType
  , selectedRange :: FilterRange
  }

data Action
  = NoAction
  | SelectedEffectType Int
  | SelectedRange Int

component :: forall q o. H.Component q Armory o Aff
component =
  H.mkComponent
    { initialState: \armory ->
        { armory
        , selectedEffectType: Nothing
        , selectedRange: genericBottom
        }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.div [ classes' "select" ]
        [ HH.select
            [ HE.onSelectedIndexChange SelectedEffectType
            ]
            ( [ HH.option_ [ HH.text "Select a weapon effect..." ] ]
                <>
                  ( Search.allFilterEffectTypes <#> \effectType ->
                      HH.option_ [ HH.text $ display effectType ]
                  )
            )
        ]
    , HH.div [ classes' "select" ]
        [ HH.select
            [ HE.onSelectedIndexChange SelectedRange
            ]
            ( Search.allFilterRanges <#> \filterRange ->
                HH.option_ [ HH.text $ display filterRange ]
            )
        ]
    ]

handleAction :: forall cs o. Action → H.HalogenM State Action cs o Aff Unit
handleAction = case _ of
  NoAction -> pure unit
  SelectedEffectType idx -> do
    if idx == 0 then do
      H.modify_ \s -> s { selectedEffectType = Nothing }
    -- Console.log $ "idx " <> show idx
    else do
      let arrayIndex = idx - 1
      let
        effectType = Arr.index Search.allFilterEffectTypes arrayIndex `unsafeFromJust`
          ("Invalid effect type index: " <> show arrayIndex)
      -- Console.log $ "idx " <> show idx <> ", selected: " <> display effectType
      H.modify_ \s -> s { selectedEffectType = Just effectType }
    pure unit
  SelectedRange idx -> do
    let filterRange = Arr.index Search.allFilterRanges idx `unsafeFromJust` "Invalid filter range index"
    H.modify_ \s -> s { selectedRange = filterRange }
    -- Console.log $ "idx " <> show idx <> ", selected: " <> display filterRange
    pure unit
