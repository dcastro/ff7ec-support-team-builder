module App.Button where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)

type State = { count :: Int }

data Action
  = Increment
  | Tick

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.p_
        [ HH.text $ "You clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    ]

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> do
    _ <- H.liftEffect random
    _ <- H.subscribe =<< timer Tick
    H.modify_ \st -> st { count = st.count + 1 }
  Tick -> do
    -- unsafeCoerce "TODO"
    pure unit

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ Aff.forkAff
      $ forever do
          Aff.delay $ Milliseconds 1000.0
          H.liftEffect $ HS.notify listener val
  pure emitter
