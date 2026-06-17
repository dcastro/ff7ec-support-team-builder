module CheckEffects.Main (main) where

import Prelude

import Core.Weapons.Parser (parseWeaponEffect)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Parsing (runParser)

foreign import readWeaponsValues :: String -> Effect (Array (Array String))

-- OB0/OB1/OB6/OB10 are in columns 18-21 (0-indexed)
obCols :: Array Int
obCols = [ 18, 19, 20, 21 ]

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

padRight :: Int -> String -> String
padRight n s = s <> fold (Array.replicate (max 0 (n - String.length s)) " ")

main :: Effect Unit
main = do
  rows <- readWeaponsValues "resources/weapons.json"
  let
    unknownMap :: Map String (Set String)
    unknownMap =
      Array.drop 1 rows # foldlWithIndex
        ( \rowIdx m row ->
            let
              weapon = fromMaybe "" (Array.index row 0)
              char = fromMaybe "" (Array.index row 1)
              label = if char == "" then weapon else char <> ": " <> weapon
            in
              Array.foldl
                ( \m' col ->
                    case Array.index row col of
                      Nothing -> m'
                      Just cell ->
                        cell
                          # String.split (String.Pattern "\n")
                          <#> String.trim
                          # foldlWithIndex
                              ( \lineIdx m'' line ->
                                  if line == "" || lineIdx == 0 then m''
                                  else
                                    case runParser line (parseWeaponEffect { rowId: rowIdx + 2, columnId: col + 1 }) of
                                      Right _ -> m''
                                      Left _ ->
                                        let
                                          key = groupingKey line
                                        in
                                          Map.insertWith Set.union key (Set.singleton label) m''
                              )
                              m'
                )
                m
                obCols
        )
        Map.empty
  if Map.isEmpty unknownMap then
    log "All weapon effects in weapons.json are supported by the parser."
  else do
    let entries = Map.toUnfoldable unknownMap :: Array (Tuple String (Set String))
    let maxKeyLen = Array.foldl (\mx (Tuple k _) -> max mx (String.length k)) 0 entries
    let sep = fold (Array.replicate (maxKeyLen + 14) "-")
    log $ "Found " <> show (Array.length entries) <> " unsupported effect type(s):\n"
    log $ padRight maxKeyLen "Effect type" <> "  Weapons"
    log sep
    for_ entries \(Tuple key weapons) -> do
      let ws = Set.toUnfoldable weapons :: Array String
      for_ (Array.mapWithIndex Tuple ws) \(Tuple i w) ->
        log $ padRight maxKeyLen (if i == 0 then key else "") <> "  " <> w
