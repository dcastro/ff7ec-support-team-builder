module Core.Weapons.Parser where

import Prelude

import Control.Alt ((<|>))
import Core.Database.VLatest
import Data.Array as Arr
import Data.Either (Either(..), hush)
import Data.FoldableWithIndex as F
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.Utils as String
import Parsing (runParser)
import Parsing as P
import Parsing.Combinators as P
import Parsing.String as P
import Parsing.String.Basic as P

type Result a = Either String a

type ParseResult =
  { weapons ::
      Array Weapon
  , errors :: Array String
  }

type Weapon =
  { name :: WeaponName
  , character :: CharacterName
  , source :: NonEmptyString
  , image :: NonEmptyString
  , ob0 :: ObLevel
  , ob1 :: ObLevel
  , ob6 :: ObLevel
  , ob10 :: ObLevel
  , cureAllAbility :: Boolean
  }

parseWeapons :: Array (Array String) -> ParseResult
parseWeapons =
  F.foldlWithIndex
    ( \rowIndex result row ->
        case parseWeapon rowIndex row of
          Right weapon -> result { weapons = Arr.snoc result.weapons weapon }
          Left err -> result { errors = Arr.snoc result.errors err }
    )
    { weapons: [], errors: [] }

parseWeapon :: Int -> Array String -> Result Weapon
parseWeapon rowIndex row = do
  name <- WeaponName <$> getCell 0
  character <- CharacterName <$> getCell 1
  source <- getCell 2
  ob0 <- getObLevel 18
  ob1 <- getObLevel 19
  ob6 <- getObLevel 20
  ob10 <- getObLevel 21
  image <- getCell 22
  -- Check if the 3rd S.Ability is a "cure all" slot
  thirdSupportAbility <- getCell 25
  let
    cureAllAbility = String.startsWith "All (Cure Spells)" (NES.toString thirdSupportAbility)
  pure
    { name
    , character
    , source
    , image
    , ob0
    , ob1
    , ob6
    , ob10
    , cureAllAbility
    }
  where
  rowId = rowIndex + 1

  getObLevel :: Int -> Result ObLevel
  getObLevel columnIndex = do
    let
      columnId = columnIndex + 1
    getCell columnIndex >>= parseObLevel { rowId, columnId }

  getCell :: Int -> Result NonEmptyString
  getCell columnIndex = do
    let
      columnId = columnIndex + 1
    str <-
      Arr.index row columnIndex
        `onErr`
          ("Row " <> show rowId <> " does not contain a cell at column " <> show columnId)
    NES.fromString str
      `onErr`
        ("Cell at " <> show rowId <> ":" <> show columnId <> " is empty")

infixl 0 onErr as !@

onErr :: forall a. Maybe a -> String -> Either String a
onErr ma err = case ma of
  Just a -> Right a
  Nothing -> Left err

type Coords = { rowId :: Int, columnId :: Int }

parseObLevel :: Coords -> NonEmptyString -> Result ObLevel
parseObLevel coords description = do
  let
    lines = description # NES.toString # String.lines
  let
    effects =
      lines
        # Arr.mapMaybe \line -> hush $ runParser line (parseWeaponEffect coords)
  pure
    { description
    , effects
    }

type Parser = P.Parser String

parseWeaponEffect :: Coords -> Parser WeaponEffect
parseWeaponEffect coords =
  inContext ("Coords " <> show coords) do
    _ <- P.optional $ P.try $ parseDuration <* space
    percentageOpt <- P.optionMaybe (parsePercentage <* space)
    effectType <- parseEffectType percentageOpt <* space
    range <- parseRange
    pure
      { effectType
      , range
      }

space :: Parser Unit
space = void $ P.char ' '

-- E.g. `30s`
parseDuration :: Parser Int
parseDuration = inContext "Duration" $ P.intDecimal <* P.char 's'

-- Some buffs (like Veil) have a percentage specifier
-- E.g. `30%`
parsePercentage :: Parser Percentage
parsePercentage = inContext "Percentage" $ Percentage <$> P.intDecimal <* P.char '%'

-- E.g. `(+30s)`
parseExtension :: Parser Int
parseExtension = inContext "Extension" do inParens $ (P.char '+' *> parseDuration)

{-

Types of ranges:
  * [Range: All Allies]
  * [Range: Other Allies] (not currently handled)
  * [Range: Single Ally]
  * [Range: All Enemies]
  * [Range: Single Enemy]
  * [Range: Self]
-}
parseRange :: Parser Range
parseRange =
  inContext "Range" do
    inBrackets do
      _ <- P.string "Range: "
      (P.string "All Allies" <|> P.string "All Enemies") $> All
        <|> (P.string "Single Ally" <|> P.string "Single Enemy") $> SingleTarget
        <|> P.string "Self" $> Self

parsePotency :: Parser Potency
parsePotency =
  inContext "Potency" do
    P.string "Low" $> Low
      <|> P.string "Mid" $> Mid
      <|> P.string "High" $> High
      <|> P.string "Extra High" $> ExtraHigh

parsePotencies :: Parser { base :: Potency, max :: Potency }
parsePotencies =
  inContext "Potencies" do
    inParens $ P.try parseTwo <|> parseSingle
  where
  parseTwo =
    inContext "Potency Range" do
      base <- parsePotency
      _ <- P.string " -> "
      max <- parsePotency
      pure { base, max }

  parseSingle =
    inContext "Single Potency" do
      pot <- parsePotency
      pure { base: pot, max: pot }

-- Examples:
-- * `PATK Down (+7s) (Low -> Mid)`
-- * `Veil (+8s)`
-- * `Heal`
parseEffectType :: Maybe Percentage -> Parser EffectType
parseEffectType percentageOpt =
  inContext "EffectType" do
    withPotencies "PATK Up" PatkUp
    <|> withPotencies "MATK Up" MatkUp
    <|> withPotencies "PDEF Up" PdefUp
    <|> withPotencies "MDEF Up" MdefUp
    <|> withPotencies "Fire Damage Up" FireDamageUp
    <|> withPotencies "Ice Damage Up" IceDamageUp
    <|> withPotencies "Thunder Damage Up" ThunderDamageUp
    <|> withPotencies "Earth Damage Up" EarthDamageUp
    <|> withPotencies "Water Damage Up" WaterDamageUp
    <|> withPotencies "Wind Damage Up" WindDamageUp
    <|> withoutPotencies "Veil" Veil
    <|> withoutPotencies "Provoke" Provoke
    <|> withPotencies "PATK Down" PatkDown
    <|> withPotencies "MATK Down" MatkDown
    <|> withPotencies "PDEF Down" PdefDown
    <|> withPotencies "MDEF Down" MdefDown
    <|> withPotencies "Fire Resistance Down" FireResistDown
    <|> withPotencies "Ice Resistance Down" IceResistDown
    <|> withPotencies "Thunder Resistance Down" ThunderResistDown
    <|> withPotencies "Earth Resistance Down" EarthResistDown
    <|> withPotencies "Water Resistance Down" WaterResistDown
    <|> withPotencies "Wind Resistance Down" WindResistDown
    <|> withoutPotencies "Enfeeble" Enfeeble
    <|> withoutPotencies "Stop" Stop
    <|> withoutPotencies "WeaknessAttackUp" ExploitWeakness
    <|> parseHeal
  where
  withPotencies :: String -> (Potencies -> EffectType) -> Parser EffectType
  withPotencies effectName constructor =
    inContext effectName do
      _ <- P.string effectName <* space
      _ <- parseExtension <* space
      pots <- parsePotencies
      pure $ constructor pots

  withoutPotencies :: String -> EffectType -> Parser EffectType
  withoutPotencies effectName buffType =
    inContext effectName do
      _ <- P.string effectName <* space
      _ <- parseExtension
      pure $ buffType

  parseHeal :: Parser EffectType
  parseHeal = do
    inContext "Heal" do
      _ <- P.string "Heal"
      case percentageOpt of
        Nothing -> P.fail $ "Expected 'Heal' to have a percentage"
        Just percentage -> pure $ Heal { percentage: percentage }

inContext :: forall a. String -> Parser a -> Parser a
inContext context =
  P.region \(P.ParseError message pos) ->
    P.ParseError (context <> ": " <> message) pos

inParens :: forall a. Parser a -> Parser a
inParens = P.between (P.char '(') (P.char ')')

inBrackets :: forall a. Parser a -> Parser a
inBrackets = P.between (P.char '[') (P.char ']')
