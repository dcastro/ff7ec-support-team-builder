module Core.Weapons.Parser2 (parseWeapons, ParseResult) where

import Core.Database.VLatest2
import Prelude

import Control.Alt ((<|>))
import Data.Array as Arr
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.FoldableWithIndex as F
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.Utils as String
import Data.Tuple (Tuple(..))
import Parsing (runParser)
import Parsing as P
import Parsing.Combinators as P
import Parsing.String as P
import Parsing.String.Basic as P
import Utils (unsafeFromJust)

type ParseResult =
  { weapons ::
      Array Weapon
  , errors :: Array String
  }

type Result a = Either String a

type Parser a = P.Parser String a

parseWeapons :: Array (Array String) -> ParseResult
parseWeapons lines =
  lines
    # Arr.drop 1 -- Skip the 1st row with the column headers
    # F.foldlWithIndex
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
  { obLevel: ob0 } <- getDescription 18
  { obLevel: ob1 } <- getDescription 19
  { obLevel: ob6 } <- getDescription 20
  { obLevel: ob10, atbCost } <- getDescription 21
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
    , atbCost
    , ob0
    , ob1
    , ob6
    , ob10
    , cureAllAbility
    }
  where
  rowId = rowIndex + 1

  getDescription :: Int -> Result ParsedDescription
  getDescription columnIndex = do
    let
      columnId = columnIndex + 1
    getCell columnIndex >>= parseDescription { rowId, columnId }

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

type ParsedDescription =
  { atbCost :: Int
  , obLevel :: ObLevel
  }

parseDescription :: Coords -> NonEmptyString -> Result ParsedDescription
parseDescription coords description = do
  let
    lines = description # NES.toString # String.lines
    firstLine = Arr.head lines `unsafeFromJust` "String.lines returned empty list"
  let
    effects =
      lines
        # Arr.mapMaybe \line -> hush $ runParser line (parseWeaponEffect coords)
  atbCost <- runParser firstLine (parseAtbCost coords) #
    lmap P.parseErrorMessage

  pure
    { atbCost
    , obLevel:
        { description
        , effects
        }
    }

parseAtbCost :: Coords -> Parser Int
parseAtbCost coords = do
  inContext ("Coords " <> show coords) do
    Tuple _ atbCost <- P.manyTill_ P.anyChar do
      P.char '(' *> P.intDecimal <* P.string " ATB)"
    pure atbCost

parseWeaponEffect :: Coords -> Parser WeaponEffect
parseWeaponEffect coords =
  inContext ("Coords " <> show coords) do
    effectType <- parseEffectType <* space
    range <- parseRange
    pure
      { effectType
      , range
      }

space :: Parser Unit
space = void $ P.char ' '

-- E.g. `30s`
parseDuration :: Parser Duration
parseDuration = inContext "Duration" $ Duration <$> P.intDecimal <* P.char 's'

-- Some buffs (like Veil) have a percentage specifier
-- E.g. `30%`
parsePercentage :: Parser Percentage
parsePercentage = inContext "Percentage" $ Percentage <$> P.intDecimal <* P.char '%'

-- E.g. `(+30s)`
parseExtension :: Parser Extension
parseExtension =
  inContext "Extension" do
    Extension <$>
      inParens do
        (P.char '+' *> P.intDecimal <* P.char 's')

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
parseEffectType :: Parser EffectType
parseEffectType =
  inContext "EffectType" do
    P.try (withPercentage "Heal" Heal)
    <|> P.try (withDurExtPotencies "PATK Up" PatkUp)
    <|> P.try (withDurExtPotencies "MATK Up" MatkUp)
    <|> P.try (withDurExtPotencies "PDEF Up" PdefUp)
    <|> P.try (withDurExtPotencies "MDEF Up" MdefUp)
    <|> P.try (withDurExtPotencies "Fire Damage Up" FireDamageUp)
    <|> P.try (withDurExtPotencies "Ice Damage Up" IceDamageUp)
    <|> P.try (withDurExtPotencies "Thunder Damage Up" ThunderDamageUp)
    <|> P.try (withDurExtPotencies "Earth Damage Up" EarthDamageUp)
    <|> P.try (withDurExtPotencies "Water Damage Up" WaterDamageUp)
    <|> P.try (withDurExtPotencies "Wind Damage Up" WindDamageUp)
    <|> P.try (withDurExtPercentage "Veil" Veil)
    <|> P.try (withDurExt "Provoke" Provoke)
    <|> P.try (withDurExtPotencies "PATK Down" PatkDown)
    <|> P.try (withDurExtPotencies "MATK Down" MatkDown)
    <|> P.try (withDurExtPotencies "PDEF Down" PdefDown)
    <|> P.try (withDurExtPotencies "MDEF Down" MdefDown)
    <|> P.try (withDurExtPotencies "Fire Resistance Down" FireResistDown)
    <|> P.try (withDurExtPotencies "Ice Resistance Down" IceResistDown)
    <|> P.try (withDurExtPotencies "Thunder Resistance Down" ThunderResistDown)
    <|> P.try (withDurExtPotencies "Earth Resistance Down" EarthResistDown)
    <|> P.try (withDurExtPotencies "Water Resistance Down" WaterResistDown)
    <|> P.try (withDurExtPotencies "Wind Resistance Down" WindResistDown)
    <|> P.try (withDurExt "Enfeeble" Enfeeble)
    <|> P.try (withDurExt "Stop" Stop)
    <|> withDurExtPercentage "WeaknessAttackUp" ExploitWeakness

  where
  withPercentage :: String -> ({ percentage :: Percentage } -> EffectType) -> Parser EffectType
  withPercentage effectName constructor = do
    inContext effectName do
      percentage <- parsePercentage <* space
      _ <- P.string effectName
      pure $ constructor $ { percentage }

  withDurExt :: String -> ({ durExt :: DurExt } -> EffectType) -> Parser EffectType
  withDurExt effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- P.string effectName <* space
      extension <- parseExtension
      pure $ constructor $ { durExt: { duration, extension } }

  withDurExtPercentage :: String -> ({ durExt :: DurExt, percentage :: Percentage } -> EffectType) -> Parser EffectType
  withDurExtPercentage effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      percentage <- parsePercentage <* space
      _ <- P.string effectName <* space
      extension <- parseExtension
      pure $ constructor $ { durExt: { duration, extension }, percentage }

  withDurExtPotencies :: String -> ({ durExt :: DurExt, potencies :: Potencies } -> EffectType) -> Parser EffectType
  withDurExtPotencies effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      potencies <- parsePotencies
      pure $ constructor $ { durExt: { duration, extension }, potencies }

inContext :: forall a. String -> Parser a -> Parser a
inContext context =
  P.region \(P.ParseError message pos) ->
    P.ParseError (context <> ": " <> message) pos

inParens :: forall a. Parser a -> Parser a
inParens = P.between (P.char '(') (P.char ')')

inBrackets :: forall a. Parser a -> Parser a
inBrackets = P.between (P.char '[') (P.char ']')
