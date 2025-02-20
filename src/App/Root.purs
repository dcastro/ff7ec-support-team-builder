module App.Root where

import Core.Database.VLatest
import Prelude

import App.EffectSelector as EffectSelector
import App.Results as Result
import App.Results as Results
import Core.Database as Db
import Core.Display (display)
import Core.Weapons.Search (AssignmentResult)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Array.NonEmpty as NAR
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Google.SheetsApi as SheetsApi
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import HtmlUtils (classes', displayIf)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Utils (unsafeFromJust)
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
  { db :: Db
  , teams :: Array AssignmentResult
  , selectedEffectCount :: Int
  , maxCharacterCount :: Int
  , effectSelectorIds :: NonEmptyArray Int
  , mustHaveChars :: Set CharacterName
  , excludeChars :: Set CharacterName
  }

mkInitialLoadedState :: Db -> LoadedState
mkInitialLoadedState db =
  { db
  , teams: []
  , selectedEffectCount: 0
  , maxCharacterCount: 2
  , effectSelectorIds: NA.range 0 (effectSelectorCount - 1)
  , mustHaveChars: Set.empty
  , excludeChars: Set.empty
  }
  where
  effectSelectorCount = 4

data Action
  = Initialize
  | HandleEffectSelector Int EffectSelector.Output
  | HandleResultsOutput Results.Output
  | SelectedMaxCharacterCount Int
  | AddEffectSelector MouseEvent
  | CheckedMustHaveChar CharacterName Boolean
  | CheckedExcludeChar CharacterName Boolean

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
    Loaded { db, teams, selectedEffectCount, maxCharacterCount, effectSelectorIds } ->
      HH.section [ classes' "hero is-fullheight" ]
        [ HH.section [ classes' "section" ]
            -- Contains all the effect selectors + the plus button
            [ HH.div [ classes' "columns is-mobile is-multiline" ] $
                ( NA.toArray effectSelectorIds <#> \effectSelectorId ->
                    -- Contains an effect selector
                    HH.div [ classes' "column is-one-third-fullhd is-half-widescreen is-half-desktop is-full-tablet is-full-mobile" ]
                      [ HH.div [ classes' "box", HP.style "height: 100%" ]
                          [ HH.slot
                              _effectSelector
                              effectSelectorId
                              EffectSelector.component
                              { db
                              , effectTypeMb: Nothing
                              , canBeDeleted: NA.length effectSelectorIds > 1
                              }
                              (HandleEffectSelector effectSelectorId)
                          ]
                      ]
                ) <>
                  -- Contains the plus button
                  [ displayIf (NA.length effectSelectorIds <= 10) $
                      HH.div [ classes' "column is-one-third-fullhd is-half-widescreen is-half-desktop is-full-tablet is-full-mobile" ]
                        [ HH.div [ classes' "box" ]
                            -- Single column used to center the plus button
                            [ HH.div [ classes' "columns is-mobile is-centered" ]
                                [ HH.button [ classes' "column button", HE.onClick AddEffectSelector ]
                                    [ HH.span [ classes' "icon is-large" ]
                                        [ HH.i [ classes' "fas fa-plus fa-2x" ] []
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
                [ HH.div [ classes' "column is-narrow has-text-weight-semibold" ]
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
            , HH.div [ classes' "columns is-mobile is-centered is-multiline" ] $
                [ HH.div [ classes' "column is-narrow has-text-weight-semibold" ]
                    [ HH.text "Must have: "
                    ]
                ] <>
                  ( db.allCharacterNames # Arr.fromFoldable <#> \name ->
                      HH.div [ classes' "column is-narrow" ]
                        [ HH.label [ classes' "checkbox" ]
                            [ HH.input
                                [ HP.type_ InputCheckbox
                                , HP.name "must-have-char"
                                , classes' "mr-1"
                                , HE.onChecked (CheckedMustHaveChar name)
                                ]
                            , HH.text (display name)
                            ]
                        ]
                  )

            , HH.div [ classes' "columns is-mobile is-centered is-multiline" ] $
                [ HH.div [ classes' "column is-narrow has-text-weight-semibold" ]
                    [ HH.text "Exclude: "
                    ]
                ] <>
                  ( db.allCharacterNames # Arr.fromFoldable <#> \name ->
                      HH.div [ classes' "column is-narrow" ]
                        [ HH.label [ classes' "checkbox" ]
                            [ HH.input
                                [ HP.type_ InputCheckbox
                                , HP.name "exclude-char"
                                , classes' "mr-1"
                                , HE.onChecked (CheckedExcludeChar name)
                                ]
                            , HH.text (display name)
                            ]
                        ]
                  )

            , HH.slot _results unit Results.component teams HandleResultsOutput

            , displayIf (Arr.null teams) $
                HH.div [ classes' "columns is-centered" ]
                  [ HH.div [ classes' "column is-three-fifths-desktop" ]
                      [ HH.div [ classes' "box" ]
                          [ HH.div [ classes' "columns is-centered" ]
                              [ HH.div [ classes' "column is-narrow content" ]
                                  if selectedEffectCount == 0 then
                                    [ HH.text "No results found. Please select at least 1 weapon effect."
                                    ]
                                  else
                                    [ HH.text "No results found, please try:"
                                    , HH.ul_
                                        [ HH.li_ [ HH.text "Decreasing the range of some weapon effects (Single Target instead of All)" ]
                                        , HH.li_ [ HH.text "Decreasing the base/max potencies (Mid instead of High)" ]
                                        , HH.li_ [ HH.text "Adjusting the Overboost levels" ]
                                        , HH.li_ [ HH.text "Increasing the maximum number of characters" ]

                                        ]
                                    ]
                              ]
                          ]
                      ]
                  ]

            ]

        , HH.div [ classes' "footer" ]
            [ HH.div [ classes' "content has-text-centered" ]
                [ HH.p_
                    [ HH.a
                        [ HP.href "https://www.buymeacoffee.com/diogo.castro"
                        , HP.target "_blank"
                        ]
                        [ HH.img
                            [ HP.src "https://cdn.buymeacoffee.com/buttons/v2/default-green.png"
                            , HP.alt "Buy Me A Coffee"
                            , HP.style "height: 60px !important;width: 217px !important;"
                            ]

                        ]
                    ]
                , HH.p_
                    [ HH.text "Thanks to "
                    , HH.code_ [ HH.text "unknownx" ]
                    , HH.text " for the datamines and "
                    , HH.code_ [ HH.text "doxcyn" ]
                    , HH.text " for maintaining the "
                    , HH.a
                        [ HP.href "https://docs.google.com/spreadsheets/d/1evoNzTA9veDRTvYJEMe-9F81QQ-CxUWN4mrd93kn2W4/edit?usp=sharing"
                        , HP.target "_blank"
                        ]
                        [ HH.text "Resources spreadsheet" ]
                    , HH.text "."
                    ]
                , HH.p_
                    [ HH.text "If you find a bug or would like to make a suggestion, open a New Issue "
                    , HH.a
                        [ HP.href "https://github.com/dcastro/ff7ec-team-builder/issues"
                        , HP.target "_blank"
                        ]
                        [ HH.text "here" ]
                    , HH.text "."
                    ]
                , HH.p_
                    [ HH.text "Find me as "
                    , HH.code_ [ HH.text "dc" ]
                    , HH.text " on Discord."
                    ]

                ]
            ]
        ]

handleAction :: forall o. Action → H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    H.liftAff SheetsApi.load
    H.liftAff Db.init >>= case _ of
      Nothing -> H.put FailedToLoad
      Just db -> do
        let initialState = mkInitialLoadedState db
        updateTeams initialState <#> Loaded >>= H.put
  HandleEffectSelector effectSelectorId output ->
    case output of
      EffectSelector.RaiseSelectionChanged -> do
        modifyLoadedState updateTeams

      EffectSelector.RaiseSetOwnedOb weaponName obRangeIndex -> do
        modifyLoadedState \state -> do
          state <- setOwnedOb weaponName obRangeIndex state
          updateTeams state

      EffectSelector.RaiseClosed -> do
        modifyLoadedState \state -> do
          let
            effectSelectorIds =
              ( NA.delete effectSelectorId state.effectSelectorIds
                  # NA.fromArray
              ) `unsafeFromJust` "Deleted the last effect selector"
          updateTeams $ state { effectSelectorIds = effectSelectorIds }

  HandleResultsOutput output -> do
    case output of
      Result.RaiseSetOwnedOb weaponName obRangeIndex -> do
        modifyLoadedState \state -> do
          state <- setOwnedOb weaponName obRangeIndex state
          updateTeams state

  SelectedMaxCharacterCount idx -> do
    modifyLoadedState \state -> do
      let maxCharacterCount = idx + 1
      Console.log $ "Maximum number of characters: " <> show maxCharacterCount
      updateTeams $ state { maxCharacterCount = maxCharacterCount }

  AddEffectSelector _ -> do
    modifyLoadedState \state -> do
      let newId = NA.last state.effectSelectorIds + 1
      let effectSelectorIds = NA.snoc state.effectSelectorIds newId
      updateTeams $ state { effectSelectorIds = effectSelectorIds }

  CheckedMustHaveChar charName checked -> do
    modifyLoadedState \state -> do
      if checked then
        updateTeams $ state { mustHaveChars = Set.insert charName state.mustHaveChars }
      else
        updateTeams $ state { mustHaveChars = Set.delete charName state.mustHaveChars }

  CheckedExcludeChar charName checked -> do
    modifyLoadedState \state -> do
      if checked then
        updateTeams $ state { excludeChars = Set.insert charName state.excludeChars }
      else
        updateTeams $ state { excludeChars = Set.delete charName state.excludeChars }

modifyLoadedState
  :: forall o
   . (LoadedState -> H.HalogenM State Action Slots o Aff LoadedState)
  -> H.HalogenM State Action Slots o Aff Unit
modifyLoadedState f = do
  state <- assumeLoaded <$> H.get
  state <- f state
  H.put $ Loaded state

assumeLoaded :: State -> LoadedState
assumeLoaded = case _ of
  Loaded state -> state
  Loading -> unsafeCrashWith "Expected state to be `Loaded`, but was `Loading`"
  FailedToLoad -> unsafeCrashWith "Expected state to be `Loaded`, but was `FailedToLoad`"

updateTeams :: forall o st action. LoadedState -> H.HalogenM st action Slots o Aff LoadedState
updateTeams state = do
  -- Calculate all possible teams
  filters <-
    NA.catMaybes <$>
      for state.effectSelectorIds \effectSelectorId -> do
        H.request _effectSelector effectSelectorId EffectSelector.GetFilter
  let
    teams =
      Search.applyFilters filters state.db
        # Search.search state.maxCharacterCount state.excludeChars
        # Search.filterMustHaveChars state.mustHaveChars
        # Search.filterDuplicates

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

  pure $ state
    { teams = teams
    , selectedEffectCount = Arr.length filters
    }

setOwnedOb :: forall m. MonadAff m => WeaponName -> Int -> LoadedState -> m LoadedState
setOwnedOb weaponName obRangeIndex state = do
  Console.log $ "Weapon " <> display weaponName <> " owned OB: " <> show obRangeIndex
  let
    updatedAllWeapons =
      Map.alter
        ( case _ of
            Just existingWeapon -> do
              let
                ownedOb =
                  if obRangeIndex == 0 then
                    Nothing
                  else
                    Just $ NAR.index existingWeapon.distinctObs (obRangeIndex - 1)
                      `unsafeFromJust`
                        ( "Attempted to set owned OB to #"
                            <> show obRangeIndex
                            <> " for weapon "
                            <> display weaponName
                            <> ", but weapon has fewer distinct OBs: "
                            <> show (NAR.length existingWeapon.distinctObs)
                        )
              Just existingWeapon { ownedOb = ownedOb }
            Nothing -> unsafeCrashWith $ "Attempted to set owned OB, but weapon was not found: " <> display weaponName
        )
        weaponName
        state.db.allWeapons

  let state' = state { db { allWeapons = updatedAllWeapons } }
  Console.log "Saving db to cache"
  Db.writeToCache state'.db
  pure state'
