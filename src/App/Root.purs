module App.Root where

import Prelude

import App.EffectSelector as EffectSelector
import Core.Armory (Armory)
import Core.Armory as Armory
import Core.Display (display)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import HtmlUtils (classes')
import Type.Proxy (Proxy(..))

type Slots =
  ( effectSelector :: EffectSelector.Slot Int
  )

_effectSelector = Proxy :: Proxy "effectSelector"

data State
  = Loading
  | FailedToLoad
  | Loaded { armory :: Armory }

data Action
  = Initialize
  | HandleEffectSelector EffectSelector.Output

component :: forall q i o. H.Component q i o Aff
component =
  H.mkComponent
    { initialState: \_ -> Loading
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  case state of
    Loading ->
      HH.div_
        [ HH.text "Loading..."
        ]
    FailedToLoad ->
      HH.div_
        [ HH.text "Failed to load"
        ]
    Loaded { armory } ->
      HH.div_
        [ HH.text $ "Loaded " <> show (Map.size armory.allWeapons) <> " weapons"
        , HH.div [ classes' "fixed-grid has-3-cols has-1-cols-mobile" ]
            [ HH.div [ classes' "grid" ]
                [ HH.div [ classes' "cell" ] [ HH.slot _effectSelector 0 EffectSelector.component armory HandleEffectSelector ]
                , HH.div [ classes' "cell" ] [ HH.slot _effectSelector 1 EffectSelector.component armory HandleEffectSelector ]
                , HH.div [ classes' "cell" ] [ HH.slot _effectSelector 2 EffectSelector.component armory HandleEffectSelector ]
                , HH.div [ classes' "cell" ] [ HH.slot _effectSelector 3 EffectSelector.component armory HandleEffectSelector ]
                , HH.div [ classes' "cell" ] [ HH.slot _effectSelector 4 EffectSelector.component armory HandleEffectSelector ]
                , HH.div [ classes' "cell" ] [ HH.slot _effectSelector 5 EffectSelector.component armory HandleEffectSelector ]
                ]
            ]
        ]

handleAction :: forall o. Action → H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    H.liftAff Armory.init >>= case _ of
      Just armory -> H.put $ Loaded { armory }
      Nothing -> H.put FailedToLoad
  HandleEffectSelector output ->
    case output of
      EffectSelector.SelectionChanged -> do
        Console.log "Selection changed"

        -- Calculate all possible teams
        responses <- H.requestAll _effectSelector EffectSelector.GetFilterResult
        let filterResults = Arr.fromFoldable $ Map.values responses
        let combinations = Search.combinations filterResults
        let teams = combinations # Arr.mapMaybe (Search.assignWeaponsToCharacters 2)

        Console.log "-----------------------------------------"
        Console.log "-----------------------------------------"
        Console.log "-----------------------------------------"
        for_ teams \team -> do
          Console.log "-----------"
          for_ team.characters \char ->
            case char.offHand of
              Just offHand -> Console.log $ display char.name <> ": " <> display char.mainHand.weapon.name <> " / " <> display offHand.weapon.name
              Nothing -> Console.log $ display char.name <> ": " <> display char.mainHand.weapon.name
          pure unit
