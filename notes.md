## FF7 Ever Crisis Weapons databases

* [[Resources] FF7 Ever Crisis Collection Tracker](https://docs.google.com/spreadsheets/d/1evoNzTA9veDRTvYJEMe-9F81QQ-CxUWN4mrd93kn2W4/): `doxcyn` updates this sheet with the information datamined by `UnknownX`
  * It's very well maintained, and the format is consistent and parser-friendly.
* [[Public] FF7 Ever Crisis Collection Tracker v1.53](https://docs.google.com/spreadsheets/d/1vPs3KRJM_7VFm2hOCCT5ZyYCrtP9ov-zbbns4JpyaaA/): Created by `Pyree`, maintained by `doxcyn`. This sheet mainly uses data from the "Resources" sheet.
  * It's also very well maintained, but the format is much harder to parse. It's more user-friendly than parser-friendly.
* [schau1's weaponData.csv](https://github.com/schau1/ff7ec/blob/main/weaponData.csv): This is the CSV used by <https://schau1.github.io/ff7ec/>.
  * It may take a few days to be updated, though the author accepts PRs.


## Decisions

* We do not consider Ultimate Weapons; they're usually not a reliable way of keeping buffs/debuffs applied throughout a battle.
* We consider "Healing Weapons" as weapons that have either a `All (Cure Spells)` S. Ability or a C. Ability that heals for at least 30 (@(ref:heal-threshold)).
    * This last criterion is used to exclude weapons like Microlaser and Flower Vase, but include Lifeguard Wraps (38% at OB0).
* We don't discard teams that are superset of other teams.
    * Say you filter by `PATK Up All + MATK Up All`. Among the possible teams, you'll find these:
        1. Cait Sith with X
        1. Cait Sith with X + Barret with Y
    * At first it might seem like the 2nd team is redundant: if Cait Sith can do it all with one
    weapon, why force Barret into your team?
      However, Cait's weapon may have high potency for `PATK Up` and low potency for `MATK Up`, whereas Barret's might have high `MATK Up`, in which case it makes sense to have both.
* When sorting the possible teams, the highest criterion is to have as high potency as possible.
  Fitting many effects into fewer characters is an important criterion, but not as important as having high potency.
  E.g. Cait Sith's Trumpet Shell can technically do `PATK Up All + MATK Up All`, but it's terrible at both.
  So having 2 characters with 1 weapon each, with high potency, is better than running Cait's Trumpet Shell.
* By default, we ignore event weapons, since their effects have usually low potency.
  Including event weapons in the calculations usually leads to a lot of noise in the results.

## Notable weapons

List of weapons that escape the norm:

* Most weapons, for each effect, have exactly 1 range/potencies.
  Yuffie's Arctic Star, however, has PATK Up High SingleTarget + PATK Up Mid->High Self.
  Silver Megaphone has PDEF Down Low SingleTarget + PDEF Down High SingleTarget (Condition: Critical Hit)
* Most weapons have either a C.Ability with healing _or_ have a Cure All S. Ability.
  But Aerith's "Umbrella" does Single Target Heal _and_ has a Cure All S. Ability.

## Invariants

These are the invariants the codebase relies on. Each one is enforced and/or assumed at
one or more sites in the code.

* **A weapon's effects are listed in the same order at every overboost level.** @(ref:effects-same-order)
    * The `effects` array is parsed independently for OB0/OB1/OB6/OB10. We assume the Nth
      effect is the "same kind" of effect (same constructor) at every level, so we can zip
      the four levels together and read how a single effect's range/potencies change as
      the weapon is overboosted.
    * `getDistinctObs` enforces this: it crashes if two levels list their effects in a
      different order. `groupsForWeapon'` / `groupForWeaponEffect` then rely on it.

* **Every effect that has potencies also has a range.** @(ref:potencies-have-range)
    * Some effects have a range but no potencies (e.g. `Heal`, `Provoke`, `Veil`), and a
      few have neither (e.g. `IncreaseCommandGauge`, Sigil effects) - but none have
      potencies _without_ a range.
    * Because of this, potencies are stored grouped by range (`GroupedWeaponRange`). When
      an effect has no range we can safely conclude it has no potencies either: e.g.
      `groupForWeaponEffect` drops the potencies along with the (absent) range, and
      `findMatchingWeapons` returns `Nothing` potencies when there are no ranges.

* **`UserStateWeapon.ownedOb` matches one of the weapon's `distinctObs`.** @(ref:owned-ob-invariant)
    * The OB range the user owns must be one of the distinct OB ranges computed for that
      weapon. When a weapon's `distinctObs` changes - e.g. after adding support for a new
      effect that splits `OB0-10` into `OB0-5` / `OB6-10` - `createDbState` resets any
      `ownedOb` that no longer matches.
