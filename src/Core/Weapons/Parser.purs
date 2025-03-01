module Core.Weapons.Parser where

import Core.Database.Types
import Core.Database.UserState.VLatest
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
  { obLevel: ob10, atbCost, commandAbilitySigil } <- getDescription 21
  image <- getCell 22

  rAbility1 <- getCell 3
  rAbility2 <- getCell 5

  sAbility1 <- getCell 23
  sAbility2 <- getCell 24
  sAbility3 <- getCell 25

  let
    -- Check if the 3rd S.Ability is a "cure all" slot
    cureAllAbility = String.startsWith "All (Cure Spells)" (NES.toString sAbility3)
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
    , commandAbilitySigil
    , sAbilities: { slot1: sAbility1, slot2: sAbility2, slot3: sAbility3 }
    , rAbilities: { slot1: rAbility1, slot2: rAbility2 }
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
  , commandAbilitySigil :: Maybe Sigil
  }

parseDescription :: Coords -> NonEmptyString -> Result ParsedDescription
parseDescription coords description = do
  let
    lines = description # NES.toString # String.lines
    firstLine = Arr.head lines `unsafeFromJust` "String.lines returned empty list"
  let
    -- Parse weapon effects, discarding parser failures
    effects =
      lines
        # Arr.mapMaybe \line -> hush $ runParser line (parseWeaponEffect coords)
  -- Every weapon must have an ATB cost
  atbCost <- runParser firstLine (parseAtbCost coords) #
    lmap P.parseErrorMessage

  -- A weapon may have a C. Ability Sigil.
  let commandAbilitySigil = hush $ runParser firstLine (parseCommandAbilitySigil coords)

  pure
    { atbCost
    , commandAbilitySigil
    , obLevel:
        { description
        , effects
        }
    }

parseAtbCost :: Coords -> Parser Int
parseAtbCost coords = do
  inContext ("Coords " <> show coords) do
    inContext "ATB Cost" do
      Tuple _ atbCost <- P.manyTill_ P.anyChar do
        P.char '(' *> P.intDecimal <* P.string " ATB)"
      pure atbCost

-- | Example input:
-- "Rolling Claw [Sigils: 1 \\◆] (3 ATB)"
parseCommandAbilitySigil :: Coords -> Parser Sigil
parseCommandAbilitySigil coords = do
  inContext ("Coords " <> show coords) do
    inContext "C. Ability Sigil" do
      Tuple _ sigil <- P.manyTill_ P.anyChar do
        P.string "[Sigils: " *> P.intDecimal *> P.string " \\" *> parseSigilSymbol <* P.string "]"
      pure sigil

-- | Example input:
-- "✖ Sigil Boost I"
parseSAbilitySigilBoost :: Parser Sigil
parseSAbilitySigilBoost =
  parseSigilSymbol <* P.string " Sigil Boost"

parseSigilSymbol :: Parser Sigil
parseSigilSymbol =
  (P.char '◆' $> SigilDiamond)
    <|> ((P.char '⬤' <|> P.char '⏺') $> SigilO)
    <|> (P.char '✖' $> SigilX)
    <|> (P.char '▲' $> SigilTriangle)

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
parseWeaponEffect :: Coords -> Parser WeaponEffect
parseWeaponEffect coords =
  inContext ("Coords " <> show coords) do
    inContext "WeaponEffect" do
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
      -- NOTE: The effect on "Blue Daffodil Gloves" is named "WeaknessAttackUp",
      -- but on "Bird of Prey" it's named "Exploit Weakness".
      <|> P.try (withDurExtPercentage "WeaknessAttackUp" ExploitWeakness)
      <|> P.try (withDurExtPercentage "Exploit Weakness" ExploitWeakness)
  where
  withPercentage :: String -> ({ range :: Range, percentage :: Percentage } -> WeaponEffect) -> Parser WeaponEffect
  withPercentage effectName constructor = do
    inContext effectName do
      percentage <- parsePercentage <* space
      _ <- P.string effectName <* space
      range <- parseRange
      pure $ constructor $ { range, percentage }

  withDurExt :: String -> ({ range :: Range, durExt :: DurExt } -> WeaponEffect) -> Parser WeaponEffect
  withDurExt effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      range <- parseRange
      pure $ constructor $ { range, durExt: { duration, extension } }

  withDurExtPercentage :: String -> ({ range :: Range, durExt :: DurExt, percentage :: Percentage } -> WeaponEffect) -> Parser WeaponEffect
  withDurExtPercentage effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      percentage <- parsePercentage <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      range <- parseRange
      pure $ constructor $ { range, durExt: { duration, extension }, percentage }

  withDurExtPotencies :: String -> ({ range :: Range, durExt :: DurExt, potencies :: Potencies } -> WeaponEffect) -> Parser WeaponEffect
  withDurExtPotencies effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      potencies <- parsePotencies <* space
      range <- parseRange
      pure $ constructor $ { range, durExt: { duration, extension }, potencies }

inContext :: forall a. String -> Parser a -> Parser a
inContext context =
  P.region \(P.ParseError message pos) ->
    P.ParseError (context <> ": " <> message) pos

inParens :: forall a. Parser a -> Parser a
inParens = P.between (P.char '(') (P.char ')')

inBrackets :: forall a. Parser a -> Parser a
inBrackets = P.between (P.char '[') (P.char ']')
