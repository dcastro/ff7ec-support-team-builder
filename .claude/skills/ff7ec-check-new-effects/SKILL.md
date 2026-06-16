---
name: ff7ec-check-new-effects
description: Check for weapon effects in FF7 Ever Crisis that the team builder app doesn't yet support. Use this skill whenever the user asks to check for new effects, wants to see what weapon effects are missing from the app, wants to sync weapon data, or says something like "any new effects?", "check for new effects", "what effects are unsupported", or "update the weapon data".
---

# FF7EC: Check for Unsupported Weapon Effects

## Steps

### 1. Download fresh weapon data

Fetch the `FF7EC_GOOGLE_SHEETS_KEY` environment variable from my nushell config, and then run:

```bash
just regen-weapons
```

### 2. Run the effect checker

```bash
just check-effects
```

This compiles and runs `src/CheckEffects/Main.purs`, which directly calls `parseWeaponEffect` from `src/Core/Weapons/Parser.purs` on every effect line in the data. Any line that fails to parse is reported.

### 3. Present the results

If there are no unsupported effects, say so briefly.

If there are unsupported effects, display the table from the script output and note that each row is an effect type (numbers normalized to `N`) with the weapons that have it.

Offer to help add support for any of them to the parser.

## How the checker works

- Each OB column cell contains one effect per line. Line 0 is the ability name/ATB cost and is skipped.
- Lines starting with `+N ATB` are ATB bonus lines — skipped.
- Every remaining line is run through `parseWeaponEffect` directly — no duplicate keyword list to maintain.
- Lines that fail to parse are reported, grouped by normalized key (numbers replaced with `N`).
