module App.Root where

import Prelude

import App.EffectSelector as EffectSelector
import App.Results as Results
import Core.Armory (Armory, Filter)
import Core.Armory as Armory
import Core.Display (display)
import Core.Weapons.Search (AssignmentResult)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (classes')
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

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
  , effectSelectorCount :: Int
  }

data Action
  = Initialize
  | HandleEffectSelector EffectSelector.Output
  | SelectedMaxCharacterCount Int
  | AddEffectSelector MouseEvent

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
    Loaded { armory, teams, maxCharacterCount, effectSelectorCount } ->
      HH.div_
        [ HH.section [ classes' "section" ]
            [ HH.div [ classes' "" ]
                [ HH.div [ classes' "grid is-col-min-25" ] $
                    ( Arr.range 0 (effectSelectorCount - 1) <#> \index ->
                        HH.slot _effectSelector index EffectSelector.component { armory, effectTypeMb: Nothing } HandleEffectSelector
                    ) <>
                      [ HH.div [ classes' "cell" ]
                          [ HH.div [ classes' "box", HE.onClick AddEffectSelector ]
                              [ HH.div [ classes' "columns is-mobile is-centered" ]
                                  [ HH.button [ classes' "column button" ]
                                      [ HH.span [ classes' "icon is-large" ]
                                          [ HH.i [ classes' "fas fa-plus fa-2x" ] []
                                          ]
                                      ]
                                  ]
                              ]
                          ]
                      ]
                ]
            ]
        , HH.section [ classes' "section" ]
            [ HH.h1 [ classes' "title is-2 has-text-centered" ] [ HH.text "Teams " ]
            , HH.div [ classes' "columns is-mobile is-centered is-vcentered" ]
                [ HH.div [ classes' "column is-narrow" ]
                    [ HH.text "Maximum number of characters: "
                    ]
                , HH.div [ classes' "column is-narrow" ]
                    [ HH.div [ classes' "select" ]
                        [ HH.select
                            [ HE.onSelectedIndexChange SelectedMaxCharacterCount
                            ]
                            [ HH.option [ HP.selected (maxCharacterCount == 1) ] [ HH.text "1" ]
                            , HH.option [ HP.selected (maxCharacterCount == 2) ] [ HH.text "2" ]
                            , HH.option [ HP.selected (maxCharacterCount == 3) ] [ HH.text "3" ]
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
            , effectSelectorCount: 4
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

  AddEffectSelector _ -> do
    state <- assumeLoaded <$> H.get
    H.put $ Loaded $ state { effectSelectorCount = state.effectSelectorCount + 1 }

assumeLoaded :: State -> LoadedState
assumeLoaded = case _ of
  Loaded state -> state
  Loading -> unsafeCrashWith "Expected state to be `Loaded`, but was `Loading`"
  FailedToLoad -> unsafeCrashWith "Expected state to be `Loaded`, but was `FailedToLoad`"

updateTeams :: forall o. State -> H.HalogenM State Action Slots o Aff State
updateTeams state = do
  let loadedState = assumeLoaded state
  -- Calculate all possible teams
  responses <- H.requestAll _effectSelector EffectSelector.GetFilter
  let filters = Arr.fromFoldable $ Map.values responses :: Array Filter
  let teams = Search.search loadedState.maxCharacterCount filters loadedState.armory

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
