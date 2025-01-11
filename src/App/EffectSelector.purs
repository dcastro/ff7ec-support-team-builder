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
import Web.Event.Event (Event)

type State =
  { armory ::
      Armory
  , selectedEffectType :: Maybe FilterEffectType
  , selectedRange :: FilterRange
  }

data Action
  = NoAction
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
            [ HE.onSelectedIndexChange SelectedRange
            ]
            ( Search.allFilterRanges <#> \filterRange ->
                HH.option_ [ HH.text $ display filterRange ]
            )
        ]
    ]

-- HH.div_
--   [ HH.div [ classes' "dropdown is-active" ]
--       [ HH.div [ classes' "dropdown-trigger" ]
--           [ HH.button [ classes' "button" ]
--               [ HH.span_ [ HH.text "Dropdown button" ]
--               , HH.span
--                   [ classes' "icon is-small" ]
--                   [ HH.i [ classes' "fas fa-angle-down" ] []
--                   ]
--               ]
--           ]
--       , HH.div [ classes' "dropdown-menu" ]
--           [ HH.div [ classes' "dropdown-content" ]
--               [ HH.a [ classes' "dropdown-item" ] [ HH.text "aa" ]
--               , HH.a [ classes' "dropdown-item" ] [ HH.text "bb" ]
--               , HH.a [ classes' "dropdown-item" ] [ HH.text "cc" ]
--               ]
--           ]
--       ]
--   ]

handleAction :: forall cs o. Action → H.HalogenM State Action cs o Aff Unit
handleAction = case _ of
  NoAction -> pure unit
  SelectedRange idx -> do
    let filterRange = unsafePartial $ Arr.unsafeIndex Search.allFilterRanges idx
    H.modify_ \s -> s { selectedRange = filterRange }
    pure unit
