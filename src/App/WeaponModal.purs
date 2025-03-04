module App.WeaponModal where

import Core.Database.Types
import Prelude

import Core.Database.UserState.VLatest (FromOb(..), ObRange(..), ToOb(..))
import Core.Display (display)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.Utils as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import HtmlUtils (classes')
import Utils (predCyclic, succCyclic)
import Web.Event.Event as E
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type Slot id = H.Slot Query Output id

type Input =
  { weapon :: Weapon
  , ownedOb :: Maybe ObRange
  }

type State =
  { weapon :: Weapon
  , selectedOb :: FromOb
  }

data Output = ModalClosed

data Action
  = CloseModal
  | Initialize
  | HandleKey H.SubscriptionId KE.KeyboardEvent
  | ChangedObSelection FromOb
  | SuccOb
  | PredOb

data Query :: forall k. k -> Type
data Query a

component :: H.Component Query Input Output Aff
component =
  H.mkComponent
    { initialState: \{ weapon, ownedOb } ->
        { weapon
        , selectedOb: pickBestOb ownedOb
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { weapon, selectedOb } =
  HH.div
    [ classes' "modal is-active"
    -- By default, the modal is 40rem wide.
    , HP.style "--bulma-modal-content-width: 50rem"
    ]
    [ HH.div [ classes' "modal-background", HE.onClick \_ -> CloseModal ] []
    , HH.div [ classes' "modal-card" ]
        [ HH.header [ classes' "modal-card-head pt-2 pb-2" ]
            -- By default, `columns` are offset up (with a negative margin-top).
            -- We reset its margins here to make them vertically centered within the `modal-card-head`.
            [ HH.div [ classes' "modal-card-title columns is-vcentered mb-0 mt-0" ]
                [ HH.div [ classes' "column is-narrow" ] [ HH.img [ HP.src (display weapon.image), classes' "image is-32x32" ] ]
                , HH.div [ classes' "column is-narrow" ] [ HH.text $ display weapon.name ]
                ]
            , HH.button [ classes' "delete", HE.onClick \_ -> CloseModal ]
                []
            ]
        , HH.section
            [ classes' "modal-card-foot"
            -- By default, the contents of `modal-card-foot` are displayed using `display: flex`
            , HP.style "display: block"
            ]
            [ HH.div_ [ HH.p [ classes' "title is-4 mb-1 " ] [ HH.text "C. Ability" ] ]
            , HH.div [ classes' "tabs is-fullwidth" ]
                [ HH.ul_ do
                    let isActive ob = if ob == selectedOb then "is-active" else ""
                    [ HH.li [ classes' "is-clickable", HE.onClick \_ -> PredOb ]
                        [ HH.a [ HP.style "border-bottom: none" ] [ HH.i [ classes' "fas fa-chevron-left" ] [] ] ]
                    , HH.li [ classes' (isActive FromOb0), HE.onClick \_ -> ChangedObSelection FromOb0 ] [ HH.a_ [ HH.span_ [ HH.text "OB0" ] ] ]
                    , HH.li [ classes' (isActive FromOb1), HE.onClick \_ -> ChangedObSelection FromOb1 ] [ HH.a_ [ HH.span_ [ HH.text "OB1" ] ] ]
                    , HH.li [ classes' (isActive FromOb6), HE.onClick \_ -> ChangedObSelection FromOb6 ] [ HH.a_ [ HH.span_ [ HH.text "OB6" ] ] ]
                    , HH.li [ classes' (isActive FromOb10), HE.onClick \_ -> ChangedObSelection FromOb10 ] [ HH.a_ [ HH.span_ [ HH.text "OB10" ] ] ]
                    , HH.li [ classes' "is-clickable", HE.onClick \_ -> SuccOb ]
                        [ HH.a [ HP.style "border-bottom: none" ] [ HH.i [ classes' "fas fa-chevron-right" ] [] ] ]
                    ]
                ]
            , HH.div [ classes' "mb-5" ]
                [ renderObLevel $
                    case selectedOb of
                      FromOb0 -> weapon.ob0
                      FromOb1 -> weapon.ob1
                      FromOb6 -> weapon.ob6
                      FromOb10 -> weapon.ob10
                ]
            , HH.div [ classes' "columns" ]
                [ HH.div [ classes' "column " ]
                    [ HH.div_
                        [ HH.p [ classes' "title is-4 mb-1" ] [ HH.text "S. Abilities" ]
                        , renderAbility weapon.sAbilities.slot1
                        , renderAbility weapon.sAbilities.slot2
                        , renderAbility weapon.sAbilities.slot3
                        ]
                    ]
                , HH.div [ classes' "column " ]
                    [ HH.div_
                        [ HH.p [ classes' "title is-4 mb-1" ] [ HH.text "R. Abilities" ]
                        , renderAbility weapon.rAbilities.slot1
                        , renderAbility weapon.rAbilities.slot2
                        ]
                    ]
                ]
            ]
        ]
    ]

renderObLevel :: forall w i. ObLevel -> HH.HTML w i
renderObLevel { description } =
  HH.div_ $
    description
      # NES.toString
      # String.lines
      <#> \line -> HH.p [] [ HH.text line ]

renderAbility :: forall w i. NonEmptyString -> HH.HTML w i
renderAbility ability =
  HH.p [] [ HH.text $ NES.toString ability ]

handleAction :: forall cs. Action → H.HalogenM State Action cs Output Aff Unit
handleAction = case _ of
  CloseModal ->
    H.raise ModalClosed
  Initialize -> do
    document <- H.liftEffect $ Window.document =<< HTML.window
    H.subscribe' \sid ->
      QE.eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
  HandleKey sid ev
    | KE.key ev == "Escape" -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        H.unsubscribe sid
        handleAction CloseModal
    | KE.key ev == "ArrowLeft" -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        handleAction PredOb
    | KE.key ev == "ArrowRight" -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        handleAction SuccOb
    | otherwise -> do
        pure unit
  ChangedObSelection selectedOb -> H.modify_ \s -> s { selectedOb = selectedOb }
  PredOb ->
    H.modify_ \s -> s { selectedOb = predCyclic s.selectedOb }
  SuccOb ->
    H.modify_ \s -> s { selectedOb = succCyclic s.selectedOb }

pickBestOb :: Maybe ObRange -> FromOb
pickBestOb = case _ of
  Nothing -> FromOb10
  Just (ObRange range) ->
    case range.to of
      ToOb0 -> FromOb0 --   If a range is `OB0-0`,                              then, by default, we'll display the info for OB0.
      ToOb5 -> FromOb1 --   If a range is `OB0-5`/`OB1-5`,                      then, by default, we'll display the info for OB1.
      ToOb9 -> FromOb6 --   If a range is `OB0-9`/`OB1-9`/`OB6-9`,              then, by default, we'll display the info for OB6.
      ToOb10 -> FromOb10 -- If a range is `OB0-10`/`OB1-10`/`OB6-10`/`OB10-10`, then, by default, we'll display the info for OB10.
