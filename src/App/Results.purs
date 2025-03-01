module App.Results where

import Core.Database.Types
import Core.Database.UserState.VLatest
import Prelude

import App.WeaponModal as WeaponModal
import Core.Display (display)
import Core.Weapons.Search (AssignmentResult)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Array.NonEmpty as NAR
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (classes', displayIf)
import Type.Prelude (Proxy(..))

teamsDisplayLimit :: Int
teamsDisplayLimit = 100

type Slot id = forall query. H.Slot query Output id

type Slots =
  ( weaponModal :: WeaponModal.Slot Unit
  )

_weaponModal = Proxy :: Proxy "weaponModal"

type State =
  { teams :: Array AssignmentResult
  , weaponForModal :: Maybe Weapon
  }

type Input = Array AssignmentResult

data Output = RaiseSetOwnedOb WeaponName Int

data Action
  = Receive Input
  | SetOwnedOb WeaponName Int
  | SelectedWeaponForModal Weapon
  | HandleWeaponModal WeaponModal.Output

component :: forall q. H.Component q Input Output Aff
component =
  H.mkComponent
    { initialState: \teams ->
        { teams
        , weaponForModal: Nothing
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div_
    [ displayIf (Arr.length state.teams > teamsDisplayLimit) $
        HH.div
          [ classes' "has-text-centered has-text-weight-semibold mb-4" ]
          [ HH.text $ "Displaying first " <> show teamsDisplayLimit <> " teams." ]
    , HH.div [ classes' "columns is-centered" ]
        [ HH.div [ classes' "column is-three-fifths-desktop" ] $
            state.teams # Arr.take teamsDisplayLimit <#> \team ->
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
                              -- Align the img/wepon name/controls vertically.
                              -- `is-1` for a smaller gap between the elements.
                              [ HH.div [ classes' "columns is-mobile is-centered is-vcentered is-1" ]
                                  [ HH.div
                                      [ classes' "column is-narrow is-clickable"
                                      , HE.onClick $ \_ -> SelectedWeaponForModal equipedWeapon.weaponData.weapon
                                      ]
                                      [ HH.img [ HP.src (display equipedWeapon.weaponData.weapon.image), classes' "image is-32x32" ]
                                      ]
                                  , HH.div
                                      [ classes' "column is-narrow is-clickable"
                                      , HE.onClick $ \_ -> SelectedWeaponForModal equipedWeapon.weaponData.weapon
                                      ]
                                      [ HH.span_
                                          [ HH.text (display equipedWeapon.weaponData.weapon.name)
                                          ]
                                      ]
                                  , HH.div [ classes' "column is-narrow" ]
                                      [ HH.div [ classes' "select" ]
                                          [ HH.select
                                              [ HE.onSelectedIndexChange (SetOwnedOb equipedWeapon.weaponData.weapon.name) ]
                                              ( [ HH.option
                                                    [ HP.selected (equipedWeapon.weaponState.ownedOb == Nothing) ]
                                                    [ HH.text "N/A" ]
                                                ]
                                                  <>
                                                    ( NAR.toArray equipedWeapon.weaponData.distinctObs <#> \obRange ->
                                                        HH.option
                                                          [ HP.selected (equipedWeapon.weaponState.ownedOb == Just obRange) ]
                                                          [ HH.text $ display obRange ]
                                                    )
                                              )
                                          ]
                                      ]
                                  ]
                              ]
                        )
        ]
    , case state.weaponForModal of
        Nothing -> HH.div_ []
        Just weaponForModal -> HH.slot _weaponModal unit WeaponModal.component weaponForModal HandleWeaponModal
    ]

handleAction :: Action → H.HalogenM State Action Slots Output Aff Unit
handleAction = case _ of
  Receive teams ->
    H.modify_ \s -> s { teams = teams }
  SetOwnedOb weaponName obRangeIndex ->
    H.raise $ RaiseSetOwnedOb weaponName obRangeIndex
  SelectedWeaponForModal weapon ->
    H.modify_ \s -> s { weaponForModal = Just weapon }
  HandleWeaponModal output ->
    case output of
      WeaponModal.ModalClosed ->
        H.modify_ \s -> s { weaponForModal = Nothing }
