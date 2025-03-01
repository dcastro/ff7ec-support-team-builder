module App.WeaponModal where

import Core.Database.Types
import Prelude

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
import Web.Event.Event as E
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type Slot id = H.Slot Query Output id

type Input = Weapon

type State =
  { weapon :: Weapon
  }

data Output = ModalClosed

data Action
  = CloseModal
  | Initialize
  | HandleKey H.SubscriptionId KE.KeyboardEvent

data Query :: forall k. k -> Type
data Query a

component :: H.Component Query Input Output Aff
component =
  H.mkComponent
    { initialState: \weapon -> { weapon }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { weapon } =
  HH.div
    [ classes' "modal is-active"
    -- By default, the modal is 40rem wide.
    -- We're increasing the width here to allow displaying S./R. Abilities.
    , HP.style "--bulma-modal-content-width: 60rem"
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
        , HH.section [ classes' "modal-card-foot" ]
            [ HH.div [ classes' "columns" ]
                [ HH.div [ classes' "column" ]
                    [ HH.p [ classes' "title is-4 mb-1" ] [ HH.text "C. Ability" ]
                    , renderObLevel 0 weapon.ob0
                    , renderObLevel 1 weapon.ob1
                    , renderObLevel 6 weapon.ob6
                    , renderObLevel 10 weapon.ob10
                    ]

                , HH.div [ classes' "column is-narrow" ]
                    [ HH.p [ classes' "title is-4 mb-1" ] [ HH.text "S. Abilities" ]
                    , renderAbility weapon.sAbilities.slot1
                    , renderAbility weapon.sAbilities.slot2
                    , renderAbility weapon.sAbilities.slot3
                    , HH.p [ classes' "title is-4 mb-1 mt-3" ] [ HH.text "R. Abilities" ]
                    , renderAbility weapon.rAbilities.slot1
                    , renderAbility weapon.rAbilities.slot2
                    ]
                ]
            ]

        ]

    ]

renderObLevel :: forall w i. Int -> ObLevel -> HH.HTML w i
renderObLevel ob { description } =
  HH.div [ classes' "mb-3" ] $
    [ HH.p [ classes' "subtitle is-5 mb-1" ] [ HH.text ("OB" <> show ob) ]
    , HH.section [] $
        ( description
            # NES.toString
            # String.lines
            <#> \line -> HH.p [] [ HH.text line ]
        )
    ]

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
    | otherwise -> do
        pure unit
