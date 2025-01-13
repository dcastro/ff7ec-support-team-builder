module App.Results where

import Prelude

import Core.Armory (Armory, ArmoryWeapon)
import Core.Armory (Filter, FilterEffectType(..), FilterRange(..))
import Core.Armory as Armory
import Core.Display (display)
import Core.Weapons.Search (AssignmentResult)
import Core.Weapons.Search as Search
import Core.Weapons.Types (WeaponName(..))
import Data.Array as Arr
import Data.Bounded.Generic (genericBottom)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import HtmlUtils (classes', tooltip)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Utils (unsafeFromJust)
import Web.Event.Event (Event)

type Slot id = forall query. H.Slot query Void id

type State =
  { teams :: Array AssignmentResult
  }

type Input = Array AssignmentResult

data Action =
  Receive Input

component :: forall q o. H.Component q Input o Aff
component =
  H.mkComponent
    { initialState: \teams ->
        { teams
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.h1 [ classes' "title is-2 has-text-centered" ] [ HH.text "Teams " ]
    , HH.div [ classes' "columns" ]
        [ HH.div [ classes' "column is-half is-offset-one-quarter" ] $
            state.teams <#> \team ->
              HH.div [ classes' "box" ] $
                Map.values team.characters # Arr.fromFoldable <#> \character ->
                  HH.div [ classes' "columns" ] $
                    [ HH.div [ classes' "column is-one-fifth" ]
                        [ HH.span [ classes' "tag is-medium" ] [ HH.text (display character.name) ]
                        ]
                    ]
                      <>
                        ( Search.getEquipedWeapons character <#> \weapon ->
                            HH.div [ classes' "column is-two-fifths" ]
                              [ HH.div [ classes' "columns is-mobile" ]
                                  [ HH.div [ classes' "column is-narrow" ]
                                      [ HH.img [ HP.src (display weapon.weapon.image), classes' "image is-32x32" ]
                                      ]
                                  , HH.div [ classes' "column is-narrow" ]
                                      [ HH.text (display weapon.weapon.name)
                                      ]
                                  ]
                              ]
                        )
        ]
    ]

mkTooltip :: ArmoryWeapon -> String
mkTooltip weapon =
  "OB0:\n" <> display weapon.ob0.description
    <> "\n\nOB6:\n"
    <> display weapon.ob6.description

handleAction :: forall cs o. Action → H.HalogenM State Action cs o Aff Unit
handleAction = case _ of
  Receive teams -> H.put { teams }
