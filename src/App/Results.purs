module App.Results where

import Prelude

import Core.Database.VLatest
import Core.Display (display)
import Core.Weapons.Search (AssignmentResult)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (classes', mkTooltipForWeapon, tooltip)

type Slot id = forall query. H.Slot query Output id

type State =
  { teams :: Array AssignmentResult
  }

type Input = Array AssignmentResult

data Output = RaiseIgnoreWeapon WeaponName

data Action
  = Receive Input
  | IgnoreWeapon WeaponName

component :: forall q. H.Component q Input Output Aff
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
  HH.div [ classes' "columns is-centered" ]
    [ HH.div [ classes' "column is-half-desktop" ] $
        state.teams <#> \team ->
          HH.div [ classes' "box" ] $
            Map.values team.characters # Arr.fromFoldable <#> \character ->
              HH.div [ classes' "columns" ] $
                [ HH.div [ classes' "column is-one-fifth" ]
                    [ HH.span [ classes' "tag is-medium" ] [ HH.text (display character.name) ]
                    ]
                ]
                  <>
                    ( Search.getEquipedWeapons character <#> \equipedWeapon ->
                        HH.div [ classes' "column is-two-fifths" ]
                          [ HH.div [ classes' "columns is-mobile is-centered" ]
                              [ HH.div [ classes' "column is-narrow" ]
                                  [ HH.img [ HP.src (display equipedWeapon.weaponData.weapon.image), classes' "image is-32x32" ]
                                  ]
                              , HH.div [ classes' "column is-narrow" ]
                                  [ HH.span
                                      [ tooltip (mkTooltipForWeapon equipedWeapon.weaponData.weapon), classes' "has-tooltip-top" ]
                                      [ HH.text (display equipedWeapon.weaponData.weapon.name)
                                      ]
                                  , HH.span
                                      [ tooltip "Ignore weapon", HE.onClick \_ -> IgnoreWeapon equipedWeapon.weaponData.weapon.name ]
                                      [ HH.i
                                          [ classes' "fas fa-delete-left ml-2"
                                          ]
                                          []
                                      ]
                                  -- TODO: display the applied effects next to each weapon
                                  ]
                              ]
                          ]
                    )
    ]

mkTooltip :: Weapon -> String
mkTooltip weapon =
  "OB0:\n" <> display weapon.ob0.description
    <> "\n\nOB6:\n"
    <> display weapon.ob6.description

handleAction :: forall cs. Action → H.HalogenM State Action cs Output Aff Unit
handleAction = case _ of
  Receive teams -> H.put { teams }
  IgnoreWeapon weaponName -> H.raise $ RaiseIgnoreWeapon weaponName
