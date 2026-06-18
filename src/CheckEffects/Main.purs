module CheckEffects.Main (main) where

import Prelude

import Core.Weapons.Parser (parseWeaponEffect)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Utils as StringUtils
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Parsing (runParser)

foreign import readWeaponsValues :: String -> Effect (Array (Array String))

-- OB0/OB1/OB6/OB10 description columns (0-indexed); first line is the ATB cost, not an effect
obCols :: Array Int
obCols = [ 18, 19, 20, 21 ]

-- Heart custom (cols 26-29) and Spade custom (cols 30-33); every line is an effect
customCols :: Array Int
customCols = [ 26, 27, 28, 29, 30, 31, 32, 33 ]

-- Produce a stable grouping key from a raw effect line.
-- Strips [Range: ...] and trailing (...) then normalizes numbers.
groupingKey :: String -> String
groupingKey line =
  let
    removeBrackets line = case String.indexOf (String.Pattern "[") line of
      Nothing -> line
      Just i -> String.trim (String.take i line)
    removeParens line = case String.indexOf (String.Pattern "(") line of
      Nothing -> line
      Just i -> String.trim (String.take i line)
  in
    line
      # removeBrackets
      # removeParens
      # normalizeDigits
  where
  -- Replace digit sequences with "N" for grouping:
  -- "+3% Limit Break Gauge" and "+8% Limit Break Gauge" -> "+N% Limit Break Gauge"
  normalizeDigits :: String -> String
  normalizeDigits s = go false "" (CU.toCharArray s)
    where
    isDigit c = c >= '0' && c <= '9'
    go wasDigit acc chars = case Array.uncons chars of
      Nothing -> acc
      Just { head: c, tail: cs }
        | isDigit c -> go true (if wasDigit then acc else acc <> "N") cs
        | otherwise -> go false (acc <> CU.singleton c) cs

-- Convert a header string to a GitHub-flavored markdown anchor ID.
-- Lowercases, keeps only alphanumeric/space/hyphen chars, replaces spaces with hyphens.
toAnchor :: String -> String
toAnchor s =
  CU.fromCharArray
    $ map (\c -> if c == ' ' then '-' else c)
    $ Array.filter (\c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == ' ' || c == '-')
    $ CU.toCharArray
    $ String.toLower s

-- Build deduplicated anchors matching GitHub's collision resolution:
-- when a slug appears for the Nth time, it becomes slug-(N-1).
--
-- Without this deduplication, `+N% Additional Damage` and `N Additional Damage` would both produce the same anchor, `#n-additional-damage``
buildAnchors :: Array String -> Array String
buildAnchors keys =
  ( Array.foldl
      ( \(Tuple seen result) anchor ->
          case Map.lookup anchor seen of
            Nothing -> Tuple (Map.insert anchor 1 seen) (Array.snoc result anchor)
            Just n -> Tuple (Map.insert anchor (n + 1) seen) (Array.snoc result (anchor <> "-" <> show n))
      )
      (Tuple Map.empty [])
      (map toAnchor keys)
  ) # \(Tuple _ result) -> result

wontSupportKeys :: Set String
wontSupportKeys = Set.fromFoldable
  [ "N% Crit Rate"
  , "Cancel Effect"
  , "+N% Stance Gauge"
  , "+N% Limit Break Gauge"
  , "+N% Additional Damage"
  ]

main :: Effect Unit
main = do
  rows <- readWeaponsValues "resources/weapons.json"
  let
    -- Map with: effect grouping key → weapon label → set of unparseable effect lines
    unknownMap :: Map String (Map String (Set String))
    unknownMap =
      Array.drop 1 rows # foldlWithIndex
        ( \rowIdx m row ->
            let
              weapon = fromMaybe "" (Array.index row 0)
              char = fromMaybe "" (Array.index row 1)
              label = if char == "" then weapon else char <> " - " <> weapon
              checkCols skipFirst acc cols =
                Array.foldl
                  ( \m' col ->
                      case Array.index row col of
                        Nothing -> m'
                        Just cell ->
                          cell
                            # StringUtils.lines
                            <#> String.trim
                            # foldlWithIndex
                                ( \lineIdx m'' line ->
                                    if line == "" || (skipFirst && lineIdx == 0) then m''
                                    else
                                      case runParser line (parseWeaponEffect { rowId: rowIdx + 2, columnId: col + 1 }) of
                                        Right _ -> m''
                                        Left _ ->
                                          let
                                            key = groupingKey line
                                          in
                                            Map.insertWith (Map.unionWith Set.union) key (Map.singleton label (Set.singleton line)) m''
                                )
                                m'
                  )
                  acc
                  cols
            in
              checkCols true m obCols
                # \m' -> checkCols false m' customCols
        )
        Map.empty
  if Map.isEmpty unknownMap then do
    log "All weapon effects in weapons.json are supported by the parser."
  else do
    let
      allEntries = Map.toUnfoldable unknownMap :: Array (Tuple String (Map String (Set String)))
      isWontSupport (Tuple key _) = Set.member key wontSupportKeys
      mainEntries = Array.filter (not <<< isWontSupport) allEntries
      wontSupportEntries = Array.filter isWontSupport allEntries
      -- Compute anchors for all entries in document order so deduplication matches GitHub
      allAnchors = buildAnchors ((mainEntries <> wontSupportEntries) <#> \(Tuple key _) -> key)
      mainAnchors = Array.take (Array.length mainEntries) allAnchors
      wontSupportAnchors = Array.drop (Array.length mainEntries) allAnchors
      mainWithAnchors = Array.zip mainEntries mainAnchors
      wontSupportWithAnchors = Array.zip wontSupportEntries wontSupportAnchors
    log "<!-- LTEX: enabled=false -->"
    log $ "Found " <> show (Array.length allEntries) <> " unsupported effect type(s)."
    log ""
    log "# Table of Contents"
    log ""
    for_ mainWithAnchors \(Tuple (Tuple key _) anchor) ->
      log $ "* [" <> key <> "](#" <> anchor <> ")"
    when (not Array.null wontSupportEntries) do
      log "* [Won't support](#wont-support)"
      for_ wontSupportWithAnchors \(Tuple (Tuple key _) anchor) ->
        log $ "  * [" <> key <> "](#" <> anchor <> ")"
    for_ mainWithAnchors \(Tuple (Tuple key weapons) _) -> do
      let weaponEntries = Map.toUnfoldable weapons :: Array (Tuple String (Set String))
      log ""
      log "---"
      log ""
      log $ "# " <> key
      log ""
      log "[↑ Back to top](#table-of-contents)"
      log ""
      for_ weaponEntries \(Tuple label lines) -> do
        log $ "* **" <> label <> "**"
        for_ (Set.toUnfoldable lines :: Array String) \line ->
          log $ "  * `" <> line <> "`"
    when (not Array.null wontSupportEntries) do
      log ""
      log "---"
      log ""
      log "# Won't support"
      log ""
      log "[↑ Back to top](#table-of-contents)"
      for_ wontSupportWithAnchors \(Tuple (Tuple key weapons) _) -> do
        let weaponCount = Map.size weapons
        log ""
        log $ "## " <> key
        log ""
        log $ show weaponCount <> if weaponCount == 1 then " weapon" else " weapons"
