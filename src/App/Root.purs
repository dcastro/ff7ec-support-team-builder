module App.Root where

import Prelude

import App.EffectSelector as EffectSelector
import App.Results as Results
import Core.Armory (Armory, FilterEffectType(..))
import Core.Armory as Armory
import Core.Display (display)
import Core.Weapons.Search (AssignmentResult, FilterOpts)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import HtmlUtils (classes')
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

type Slots =
  ( effectSelector :: EffectSelector.Slot Int
  , results :: Results.Slot Unit
  )

_effectSelector = Proxy :: Proxy "effectSelector"
_results = Proxy :: Proxy "results"

data State
  = Loading
  | FailedToLoad
  | Loaded LoadedState

type LoadedState =
  { armory :: Armory
  , teams :: Array AssignmentResult
  , maxCharacterCount :: Int
  }

data Action
  = Initialize
  | HandleEffectSelector EffectSelector.Output
  | SelectedMaxCharacterCount Int

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
    Loaded { armory, teams } ->
      HH.div_
        [ HH.section [ classes' "section" ]
            [ HH.div [ classes' "fixed-grid has-3-cols has-1-cols-mobile" ]
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
        , HH.section [ classes' "section" ]
            [ HH.h1 [ classes' "title is-2 has-text-centered" ] [ HH.text "Teams " ]
            , HH.div_
                [ HH.label []
                    [ HH.text
                        "Maximum number of characters: "
                    , HH.div [ classes' "select" ]
                        [ HH.select
                            [ HE.onSelectedIndexChange SelectedMaxCharacterCount
                            ]
                            [ HH.option_ [ HH.text "1" ]
                            , HH.option_ [ HH.text "2" ]
                            , HH.option_ [ HH.text "3" ]
                            ]
                        ]

                    ]

                ]
            , HH.slot_ _results unit Results.component teams
            ]
        ]

handleAction :: forall o. Action → H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    H.liftAff Armory.init >>= case _ of
      Just armory -> do
        let
          initialState =
            { armory
            , teams: []
            , maxCharacterCount: 2
            }
        Loaded initialState # updateTeams >>= H.put
      Nothing -> H.put FailedToLoad
  HandleEffectSelector output ->
    case output of
      EffectSelector.RaiseSelectionChanged -> do
        Console.log "Selection changed"
        H.get >>= updateTeams >>= H.put
      EffectSelector.RaiseCheckedIgnored weaponName ignored -> do
        H.get >>= case _ of
          Loaded state -> do
            Console.log $ "Weapon " <> display weaponName <> " ignored: " <> show ignored
            let
              updatedAllWeapons =
                Map.alter
                  ( case _ of
                      Just existingWeapon -> Just existingWeapon { ignored = ignored }
                      Nothing -> unsafeCrashWith $ "Attempted to set 'ignored' flag, but weapon was not found: " <> display weaponName
                  )
                  weaponName
                  state.armory.allWeapons

            let state' = state { armory { allWeapons = updatedAllWeapons } }
            Console.log "Saving armory to cache"
            Armory.writeToCache state'.armory

            updateTeams (Loaded state') >>= H.put
          _ ->
            unsafeCrashWith "Attempted to set 'ignored' flag before app has loaded"
  SelectedMaxCharacterCount idx -> do
    let maxCharacterCount = idx + 1
    Console.log $ "Maximum number of characters: " <> show maxCharacterCount
    state <- assumeLoaded <$> H.get
    Loaded (state { maxCharacterCount = maxCharacterCount })
      # updateTeams
      >>= H.put

assumeLoaded :: State -> LoadedState
assumeLoaded = case _ of
  Loaded state -> state
  Loading -> unsafeCrashWith "Expected state to be `Loaded`, but was `Loading`"
  FailedToLoad -> unsafeCrashWith "Expected state to be `Loaded`, but was `FailedToLoad`"

updateTeams :: forall o. State -> H.HalogenM State Action Slots o Aff State
updateTeams state = do
  let loadedState = assumeLoaded state
  -- Calculate all possible teams
  responses <- H.requestAll _effectSelector EffectSelector.GetFilterOpts
  let filterOpts = Arr.fromFoldable $ Map.values responses :: Array FilterOpts
  let teams = Search.search loadedState.maxCharacterCount filterOpts loadedState.armory

  -- Console.log "-----------------------------------------"
  -- Console.log "-----------------------------------------"
  -- Console.log "-----------------------------------------"
  -- for_ teams \team -> do
  --   Console.log "-----------"
  --   for_ team.characters \char ->
  --     case char.offHand of
  --       Just offHand -> Console.log $ display char.name <> ": " <> display char.mainHand.weapon.name <> " / " <> display offHand.weapon.name
  --       Nothing -> Console.log $ display char.name <> ": " <> display char.mainHand.weapon.name
  --   pure unit

  pure $ Loaded $ loadedState { teams = teams }
