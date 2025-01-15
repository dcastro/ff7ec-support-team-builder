module App.EffectSelector where

import Prelude

import Core.Armory (Armory, ArmoryWeapon, Filter, FilterEffectType, FilterRange)
import Core.Armory as Armory
import Core.Display (display)
import Core.Weapons.Search as Search
import Core.Weapons.Types (WeaponName)
import Data.Array as Arr
import Data.Bounded.Generic (genericBottom)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import HtmlUtils (classes', mkTooltipForWeapon, tooltip)
import Utils (unsafeFromJust)

type Slot id = H.Slot Query Output id

type Input =
  { armory :: Armory
  , effectTypeMb :: Maybe FilterEffectType
  }

type State =
  { armory ::
      Armory
  , selectedEffectType :: Maybe FilterEffectType
  , selectedRange :: FilterRange
  , matchingWeapons :: Array ArmoryWeapon
  }

data Output
  = RaiseSelectionChanged
  | RaiseCheckedIgnored WeaponName Boolean

data Action
  = SelectedEffectType Int
  | SelectedRange Int
  | CheckedIgnored WeaponName Boolean
  | Initialize
  | Receive Input

data Query a = GetFilter (Filter -> a)

component :: H.Component Query Input Output Aff
component =
  H.mkComponent
    { initialState: \{ armory, effectTypeMb } ->
        updateMatchingWeapons
          { armory
          , selectedEffectType: effectTypeMb
          , selectedRange: genericBottom
          , matchingWeapons: []
          }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [ classes' "box" ]
    [ HH.div [ classes' "columns is-mobile is-centered" ]
        [ HH.div [ classes' "column is-narrow" ]
            [ HH.div [ classes' "select" ]
                [ HH.select
                    [ HE.onSelectedIndexChange SelectedEffectType
                    ]
                    ( [ HH.option_ [ HH.text "Select a weapon effect..." ] ]
                        <>
                          ( Armory.allFilterEffectTypes <#> \effectType -> do
                              let selected = state.selectedEffectType == Just effectType
                              HH.option [ HP.selected selected ] [ HH.text $ display effectType ]
                          )
                    )
                ]
            , HH.div [ classes' "select" ]
                [ HH.select
                    [ HE.onSelectedIndexChange SelectedRange
                    ]
                    ( Armory.allFilterRanges <#> \filterRange ->
                        HH.option_ [ HH.text $ display filterRange ]
                    )
                ]

            , HH.div [ classes' "columns is-mobile is-centered" ]
                [ HH.div [ classes' "column is-narrow" ]
                    [ HH.table [ classes' "table" ]
                        [ HH.tbody_ $
                            [ HH.tr_
                                [ HH.th_ []
                                , HH.th_ [ HH.text "Weapon" ]
                                , HH.th_ [ HH.text "Character" ]
                                , HH.th_ [ HH.text "Ignored?" ]
                                ]
                            ]
                              <>
                                ( state.matchingWeapons <#> \weapon ->
                                    HH.tr_
                                      [ HH.img [ HP.src (display weapon.image), classes' "image is-32x32" ]
                                      , HH.td
                                          [ tooltip (mkTooltipForWeapon weapon), classes' "has-tooltip-right" ]
                                          [ HH.text $ display weapon.name ]
                                      , HH.td_ [ HH.text $ display weapon.character ]
                                      , HH.td_
                                          [ HH.span [ classes' "checkbox " ]
                                              [ HH.input
                                                  [ HP.type_ InputCheckbox
                                                  , HP.checked weapon.ignored
                                                  , HE.onChecked \ignored -> CheckedIgnored weapon.name ignored
                                                  ]
                                              ]
                                          ]
                                      ]
                                )
                        ]
                    ]
                ]
            ]
        ]
    ]

handleAction :: forall cs. Action → H.HalogenM State Action cs Output Aff Unit
handleAction = case _ of
  SelectedEffectType idx -> do
    if idx == 0 then
      do
        Console.log $ "Deselected effect type"
        H.modify_ \s -> s { selectedEffectType = Nothing }
          # updateMatchingWeapons
    else do
      -- Find the correct filter
      let arrayIndex = idx - 1
      let
        effectType = Arr.index Armory.allFilterEffectTypes arrayIndex `unsafeFromJust`
          ("Invalid effect type index: " <> show arrayIndex)

      Console.log $ "idx " <> show idx <> ", selected: " <> display effectType
      H.modify_ \s -> s { selectedEffectType = Just effectType }
        # updateMatchingWeapons
    H.raise RaiseSelectionChanged

  SelectedRange idx -> do
    let filterRange = Arr.index Armory.allFilterRanges idx `unsafeFromJust` "Invalid filter range index"
    Console.log $ "idx " <> show idx <> ", selected: " <> display filterRange
    H.modify_ \s -> s { selectedRange = filterRange }
      # updateMatchingWeapons
    H.raise RaiseSelectionChanged

  CheckedIgnored weaponName ignored -> do
    H.raise $ RaiseCheckedIgnored weaponName ignored

  Initialize -> do
    -- When this EffectSelector is done rendering, if the initial state has an effect type,
    -- we notify the root component so the results section will be updated.
    H.gets _.selectedEffectType >>= case _ of
      Just _ -> H.raise RaiseSelectionChanged
      Nothing -> pure unit

  Receive input -> do
    H.modify_ \state -> updateMatchingWeapons $ state { armory = input.armory }

updateMatchingWeapons :: State -> State
updateMatchingWeapons state = do
  case state.selectedEffectType of
    Just effectType -> do
      let filter = { effectType, range: state.selectedRange } :: Filter
      let filterResult = Search.findMatchingWeapons filter state.armory
      let matchingWeapons = filterResult.matchingWeaponsAndPotencies <#> \{ weapon } -> weapon
      state { matchingWeapons = matchingWeapons }
    Nothing -> state { matchingWeapons = [] }

handleQuery :: forall action a m. Query a -> H.HalogenM State action () Output m (Maybe a)
handleQuery = case _ of
  GetFilter reply -> do
    state <- H.get
    case state.selectedEffectType of
      Just effectType -> pure $ Just $ reply
        { effectType
        , range: state.selectedRange
        }
      Nothing -> pure Nothing
