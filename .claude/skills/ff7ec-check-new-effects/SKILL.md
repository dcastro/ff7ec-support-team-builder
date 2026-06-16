---
name: ff7ec-check-new-effects
description: Check for weapon effects in FF7 Ever Crisis that the team builder app doesn't yet support. Use this skill whenever the user asks to check for new effects, wants to see what weapon effects are missing from the app, wants to sync weapon data, or says something like "any new effects?", "check for new effects", "what effects are unsupported", or "update the weapon data".
---

# FF7EC: Check for Unsupported Weapon Effects

## Steps

### 1. Download fresh weapon data

```bash
just regen-weapons
```

If this fails with a missing API key error, tell the user:
> Set the `FF7EC_GOOGLE_SHEETS_KEY` environment variable with a Google Sheets API key, then run again.

### 2. Run the effect checker

```bash
python3 .claude/skills/ff7ec-check-new-effects/scripts/check_effects.py
```

This reads `resources/weapons.json`, extracts effect lines from the OB0/OB1/OB6/OB10 columns (columns 18–21), and reports any effect keywords not handled by `src/Core/Weapons/Parser.purs`.

### 3. Present the results

If there are no unsupported effects, say so briefly.

If there are unsupported effects, display the table from the script output and note that each row is an effect type (numbers normalized to `N`) with the weapons that have it. Offer to help add support for any of them to the parser.

## How the checker works

- Each OB column cell contains one effect per line. Line 0 is the ability name/ATB cost and is skipped.
- Lines starting with `+N ATB` are ATB bonus lines — skipped.
- Each remaining line is checked against the known keywords in `Parser.purs` (`parseWeaponEffect`).
- Lines whose keyword doesn't match any known effect are reported.
- Numeric values are normalized to `N` so `+1%` and `+3%` variants of the same effect type are grouped.

## Keeping the checker in sync

The `KNOWN_KEYWORDS` set in `scripts/check_effects.py` mirrors the `parseWeaponEffect` function in `src/Core/Weapons/Parser.purs`. When a new effect is added to the parser, add its keyword to that set too.
