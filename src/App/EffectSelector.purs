module App.EffectSelector where

import Prelude

import Core.Armory (Armory, ArmoryWeapon)
import Core.Armory as Armory
import Core.Display (display)
import Core.Weapons.Search (FilterEffectType(..), FilterRange(..), Filter)
import Core.Weapons.Search as Search
import Core.Weapons.Types (WeaponName(..))
import Data.Array as Arr
import Data.Bounded.Generic (genericBottom)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import HtmlUtils (classes', tooltip)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Utils (unsafeFromJust)
import Web.Event.Event (Event)

type Input = Armory

type State =
  { armory ::
      Armory
  , selectedEffectType :: Maybe FilterEffectType
  , selectedRange :: FilterRange
  , applicableWeapons :: Array ArmoryWeapon
  }

data Output =
  SelectionChanged

data Action
  = SelectedEffectType Int
  | SelectedRange Int

component :: forall q. H.Component q Input Output Aff
component =
  H.mkComponent
    { initialState: \armory ->
        updateApplicableWeapons
          { armory
          , selectedEffectType: Nothing
          , selectedRange: genericBottom
          , applicableWeapons: []
          }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
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
                  ( Search.allFilterEffectTypes <#> \effectType -> do
                      let selected = state.selectedEffectType == Just effectType
                      HH.option [ HP.selected selected ] [ HH.text $ display effectType ]
                  )
            )
        ]
    , HH.div [ classes' "select" ]
        [ HH.select
            [ HE.onSelectedIndexChange SelectedRange
            ]
            ( Search.allFilterRanges <#> \filterRange ->
                HH.option_ [ HH.text $ display filterRange ]
            )
        ]

    , HH.div_
        [ HH.table [ classes' "table" ]
            [ HH.tbody_ $
                state.applicableWeapons <#> \weapon ->
                  HH.tr_
                    [ HH.img [ HP.src (display weapon.image), classes' "image is-32x32" ]
                    , HH.td
                        [ tooltip (mkTooltip weapon), classes' "has-tooltip-right" ]
                        [ HH.text $ display weapon.name ]
                    , HH.td_ [ HH.text $ display weapon.character ]
                    ]
            ]
        ]
    ]

mkTooltip :: ArmoryWeapon -> String
mkTooltip weapon =
  "OB0:\n" <> display weapon.ob0.description
    <> "\n\nOB6:\n"
    <> display weapon.ob6.description

handleAction :: forall cs. Action → H.HalogenM State Action cs Output Aff Unit
handleAction = case _ of
  SelectedEffectType idx -> do
    if idx == 0 then
      do
        Console.log $ "Deselected effect type"
        H.modify_ \s -> s { selectedEffectType = Nothing }
          # updateApplicableWeapons
    else do
      -- Find the correct filter
      let arrayIndex = idx - 1
      let
        effectType = Arr.index Search.allFilterEffectTypes arrayIndex `unsafeFromJust`
          ("Invalid effect type index: " <> show arrayIndex)

      Console.log $ "idx " <> show idx <> ", selected: " <> display effectType
      H.modify_ \s -> s { selectedEffectType = Just effectType }
        # updateApplicableWeapons
    H.raise SelectionChanged

  SelectedRange idx -> do
    let filterRange = Arr.index Search.allFilterRanges idx `unsafeFromJust` "Invalid filter range index"
    Console.log $ "idx " <> show idx <> ", selected: " <> display filterRange
    H.modify_ \s -> s { selectedRange = filterRange }
      # updateApplicableWeapons
    H.raise SelectionChanged

updateApplicableWeapons :: State -> State
updateApplicableWeapons state = do
  case state.selectedEffectType of
    Just effectType -> do
      let filter = { effectType, range: state.selectedRange } :: Filter
      let applicableWeaponNames = Map.lookup filter state.armory.groupedByEffect # fromMaybe [] :: Array WeaponName
      let
        applicableWeapons = applicableWeaponNames <#> \weaponName ->
          Map.lookup weaponName state.armory.allWeapons `unsafeFromJust` ("Weapon name '" <> display weaponName <> "' from group '" <> show filter <> "' not found.")
      state { applicableWeapons = applicableWeapons }
    Nothing -> state { applicableWeapons = [] }
