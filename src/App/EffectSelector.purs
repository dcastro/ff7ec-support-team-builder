module App.EffectSelector where

import Prelude

import Core.Armory (Armory, ArmoryWeapon, Filter, FilterEffectType(..), FilterRange)
import Core.Armory as Armory
import Core.Display (display)
import Core.Weapons.Search (FilterResult)
import Core.Weapons.Types (WeaponName)
import Data.Array as Arr
import Data.Bounded.Generic (genericBottom)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (classes', mkTooltipForWeapon, tooltip)
import Utils (unsafeFromJust)

type Slot id = H.Slot Query Output id

type Input = Armory

type State =
  { armory ::
      Armory
  , selectedEffectType :: Maybe FilterEffectType
  , selectedRange :: FilterRange
  , matchingWeapons :: Array ArmoryWeapon
  }

data Output =
  SelectionChanged

data Action
  = SelectedEffectType Int
  | SelectedRange Int
  | Initialize

data Query a = GetFilterResult (FilterResult -> a)

component :: H.Component Query Input Output Aff
component =
  H.mkComponent
    { initialState: \armory ->
        updateMatchingWeapons
          { armory
          , selectedEffectType: Nothing
          , selectedRange: genericBottom
          , matchingWeapons: []
          }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [ classes' "box" ]
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

    , HH.div_
        [ HH.table [ classes' "table" ]
            [ HH.tbody_ $
                state.matchingWeapons <#> \weapon ->
                  HH.tr_
                    [ HH.img [ HP.src (display weapon.image), classes' "image is-32x32" ]
                    , HH.td
                        [ tooltip (mkTooltipForWeapon weapon), classes' "has-tooltip-right" ]
                        [ HH.text $ display weapon.name ]
                    , HH.td_ [ HH.text $ display weapon.character ]
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
    H.raise SelectionChanged

  SelectedRange idx -> do
    let filterRange = Arr.index Armory.allFilterRanges idx `unsafeFromJust` "Invalid filter range index"
    Console.log $ "idx " <> show idx <> ", selected: " <> display filterRange
    H.modify_ \s -> s { selectedRange = filterRange }
      # updateMatchingWeapons
    H.raise SelectionChanged

  Initialize -> do
    -- When this EffectSelector is done rendering, if the initial state has an effect type,
    -- we notify the root component so the results section will be updated.
    H.gets _.selectedEffectType >>= case _ of
      Just _ -> H.raise SelectionChanged
      Nothing -> pure unit

updateMatchingWeapons :: State -> State
updateMatchingWeapons state = do
  case state.selectedEffectType of
    Just effectType -> do
      let filter = { effectType, range: state.selectedRange } :: Filter
      let matchingWeaponNames = Map.lookup filter state.armory.groupedByEffect # fromMaybe [] :: Array WeaponName
      let
        matchingWeapons = matchingWeaponNames <#> \weaponName ->
          Map.lookup weaponName state.armory.allWeapons `unsafeFromJust` ("Weapon name '" <> display weaponName <> "' from group '" <> show filter <> "' not found.")
      state { matchingWeapons = matchingWeapons }
    Nothing -> state { matchingWeapons = [] }

handleQuery :: forall action a m. Query a -> H.HalogenM State action () Output m (Maybe a)
handleQuery = case _ of
  GetFilterResult reply -> do
    state <- H.get
    case state.selectedEffectType of
      Just effectType -> pure $ Just $ reply
        { filter:
            { effectType
            , range: state.selectedRange
            }
        , required: true
        , matchingWeapons: state.matchingWeapons
        }
      Nothing -> pure Nothing
