---
name: add-effect
description: >
  Add support for a new weapon effect to the FF7EC team builder app.
  Use this skill whenever the user invokes `/add-effect <effect name>` or asks to
  add support for a weapon effect, implement a new effect, or make a weapon effect
  stop appearing in unsupported_effects.md.
---

# Add Weapon Effect

Adds full support for a new weapon effect. The effect name comes from the `/add-effect` invocation argument, e.g. `/add-effect Fire Resistance Up`.

## Step 1 — Research

Look up the effect in `resources/unsupported_effects.md`:
- Find the section whose heading matches (or closely matches) the effect name
- Read the example lines to learn the exact string the parser must match and what the format is

Determine the **parser format** from the example lines:

| Example line shape | Parser combinator | Effect record type |
|-|-|-|
| `30s Fire Resistance Up (+6s) [Range: All Allies]` | `withDurExt` | `{ range, durExt }` |
| `40s 5% Veil (+8s) [Range: Self]` | `withDurExtPercentage` | `{ range, durExt, percentage }` |
| `16s PATK Up (+5s) (Mid -> High) [Range: All Allies]` | `withDurExtPotencies` | `{ range, durExt, potencies }` |
| `74% Heal [Range: All Allies]` | `withPercentage` | `{ range, percentage }` |

Also decide on:
- **PureScript constructor name** (CamelCase, no spaces, e.g. `FireResistUp`)
- **Filter constructor name** (prefix `Filter`, e.g. `FilterFireResistUp`)
- **Parser string** (the exact substring in the raw game text, e.g. `"Fire Resistance Up"`)
- **Display string** (human-readable, e.g. `"Fire resistance up"`)
- **Has potencies?** (true only for `withDurExtPotencies` effects)

If an effect with the same name but a different shape already exists (e.g. `IceWeakness` exists without a percentage, and you're adding `IceWeakness` WITH a percentage), choose a distinct constructor name that includes the distinguishing feature (e.g. `IceWeaknessBonus` or similar) and note this to the user.

## Step 2 — Update `src/Core/Database/Types.purs`

This file has several parallel structures that all need the new effect added. Do them in order:

### 2a. `WeaponEffect` data type (around line 139)

Add a new variant using the record type determined in Step 1:
```purescript
-- for withDurExt:
| FireResistUp { range :: Range, durExt :: DurExt }
-- for withDurExtPotencies:
| FireResistUp { range :: Range, durExt :: DurExt, potencies :: Potencies }
-- for withDurExtPercentage:
| FireResistUp { range :: Range, durExt :: DurExt, percentage :: Percentage }
-- for withPercentage:
| FireResistUp { range :: Range, percentage :: Percentage }
```

### 2b. `FilterEffectType` data type (around line 181)

Add `| FilterFireResistUp` in the logically appropriate group (buffs with buffs, debuffs with debuffs, etc.)

### 2c. `Show WeaponEffect` instance

Add a case: `FireResistUp rec -> showRec rec "FireResistUp"`

### 2d. `Show FilterEffectType` instance

Add a case: `FilterFireResistUp -> "FilterFireResistUp"`

### 2e. `WriteForeign WeaponEffect` instance

Add a case: `FireResistUp rec -> writeRecord rec "FireResistUp"`

### 2f. `WriteForeign FilterEffectType` instance

Add a case: `FilterFireResistUp -> writeImpl "FilterFireResistUp"`

### 2g. `ReadForeign WeaponEffect` instance

Add a case inside `exhaustiveWeaponEffectMatch <#> \x -> case x of`:
```purescript
FireResistUp _ -> tryRead FireResistUp recType value "FireResistUp"
```

### 2h. `ReadForeign FilterEffectType` instance

Add a case inside `allFilterEffectTypes <#> \x -> case x of`:
```purescript
FilterFireResistUp -> tryRead x str "FilterFireResistUp"
```

### 2i. `Display FilterEffectType` instance

Add a case: `FilterFireResistUp -> "Fire resistance up"`

### 2j. `allFilterEffectTypes` array AND its exhaustive check

Add `FilterFireResistUp` to **both** the array and the `_ = case _ of` exhaustive pattern match in the `where` block.

### 2k. `exhaustiveWeaponEffectMatch` array AND its exhaustive check

Add to **both** the array and the `_ = case _ of` exhaustive pattern match. Use the dummy values already defined in the `where` block:
```purescript
-- for withDurExt:
, FireResistUp { range, durExt }
-- for withDurExtPotencies:
, FireResistUp { range, durExt, potencies }
-- for withDurExtPercentage:
, FireResistUp { range, durExt, percentage }
-- for withPercentage:
, FireResistUp { range, percentage }
```

## Step 3 — Update `src/Core/Weapons/Parser.purs`

In `parseWeaponEffect`, add a new line using the correct combinator. Position it with effects of similar type (buffs with buffs, debuffs with debuffs):

```purescript
<|> P.try (withDurExt "Fire Resistance Up" FireResistUp)
```

The parser string must **exactly** match what appears in the game data. Check the example lines in `unsupported_effects.md` carefully.

## Step 4 — Update `test/Test/Core/Weapons/ParserSpec.purs`

Add one concrete test case using a real example line from `unsupported_effects.md`:

```purescript
"30s Fire Resistance Up (+6s) [Range: All Allies]"
  `shouldParse`
    FireResistUp { range: All, durExt: { duration: Duration 30, extension: Extension 6 } }
```

Pick a real example and fill in the actual values.

## Step 5 — Update `src/Core/Database.purs`

### 5a. `areWeaponEffectsEquivalent` (inside `getDistinctObs`)

Add a case. Two effects of the same type are "equivalent" (same OB slot) if their potencies AND range match — duration/extension are ignored.

```purescript
-- for withDurExt or withDurExtPercentage (no potencies):
FireResistUp { range: range1, durExt: _ } ->
  case y of
    FireResistUp { range: range2, durExt: _ } -> range1 == range2
    _ -> crash unit

-- for withDurExtPotencies (has potencies):
FireResistUp { range: range1, durExt: _, potencies: pot1 } ->
  case y of
    FireResistUp { range: range2, durExt: _, potencies: pot2 } -> pot1 == pot2 && range1 == range2
    _ -> crash unit
```

### 5b. `groupForWeaponEffect`

Add a case that determines how the effect is grouped in the database:

```purescript
-- for effects WITHOUT potencies:
FireResistUp { range } -> Just { effectType: FilterFireResistUp, range: Just range, potencies: Nothing }

-- for effects WITH potencies — must extract potencies from all 4 OB levels:
FireResistUp { range, potencies: ob0Potencies } -> case ob1, ob6, ob10 of
  FireResistUp ob1, FireResistUp ob6, FireResistUp ob10 -> Just { effectType: FilterFireResistUp, range: Just range, potencies: Just { ob0: ob0Potencies, ob1: ob1.potencies, ob6: ob6.potencies, ob10: ob10.potencies } }
  _, _, _ -> crash unit
```

## Step 6 — Update `src/App/EffectSelector.purs`

### 6a. `hasPotencies`

Add a case:
```purescript
FilterFireResistUp -> false  -- or true if effect has potencies
```

### 6b. `hasRange`

Add a case. Almost all effects have a range — set to `false` only if the effect genuinely has no range (like Sigil effects):
```purescript
FilterFireResistUp -> true
```

## Step 7 — Update `test/Test/Core/Weapons/DatabaseSpec.purs`

### 7a. `getPotencies`

Add a case:
```purescript
-- for effects WITHOUT potencies:
FireResistUp {} -> Nothing

-- for effects WITH potencies:
FireResistUp { potencies } -> Just $ FireResistUp' potencies
```

### 7b. `EffectTypeAndPotencies` data type (only if the effect HAS potencies)

Add a variant: `| FireResistUp' Potencies`

## Step 8 — Verify

Run:
```
just check-effects
```

Open `resources/unsupported_effects.md` and confirm the effect no longer appears in the list. If the effect still appears, check:
1. Does the parser string exactly match the raw game text?
2. Is the effect in `groupForWeaponEffect` — that's what puts it in the DB

If the tests fail with exhaustiveness errors, you missed one of the parallel pattern-match blocks in `Types.purs`.
