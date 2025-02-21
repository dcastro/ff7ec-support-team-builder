module App.WeaponModal where

import Core.Database.VLatest
import Prelude

import Core.Display (display)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NES
import Data.String.Utils as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import HtmlUtils (classes')

type Slot id = H.Slot Query Output id

type Input = Weapon

type State =
  { weapon :: Weapon
  }

data Output = ModalClosed

data Action = CloseModal

data Query :: forall k. k -> Type
data Query a

component :: H.Component Query Input Output Aff
component =
  H.mkComponent
    { initialState: \weapon -> { weapon }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Nothing
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { weapon } =
  HH.div [ classes' "modal is-active" ]
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
            [ HH.p []
                [ renderObLevel 0 weapon.ob0
                , renderObLevel 1 weapon.ob1
                , renderObLevel 6 weapon.ob6
                , renderObLevel 10 weapon.ob10
                ]
            ]

        ]

    ]

renderObLevel :: forall w i. Int -> ObLevel -> HH.HTML w i
renderObLevel ob { description } =
  HH.div [ classes' "mb-3" ] $
    [ HH.p [ classes' "title is-5 mb-1" ] [ HH.text ("OB" <> show ob) ]
    , HH.section [] $
        ( description
            # NES.toString
            # String.lines
            <#> \line -> HH.p [] [ HH.text line ]
        )
    ]

handleAction :: forall cs. Action → H.HalogenM State Action cs Output Aff Unit
handleAction = case _ of
  CloseModal -> H.raise ModalClosed
