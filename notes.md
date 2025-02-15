## FF7 Ever Crisis Weapons databases

* [[Resources] FF7 Ever Crisis Collection Tracker](https://docs.google.com/spreadsheets/d/1evoNzTA9veDRTvYJEMe-9F81QQ-CxUWN4mrd93kn2W4/): `doxcyn` updates this sheet with the information datamined by `UnknownX`
  * It's very well maintained, and the format is consistent and parser-friendly.
* [[Public] FF7 Ever Crisis Collection Tracker v1.53](https://docs.google.com/spreadsheets/d/1vPs3KRJM_7VFm2hOCCT5ZyYCrtP9ov-zbbns4JpyaaA/): Created by `Pyree`, maintained by `doxcyn`. This sheet mainly uses data from the "Resources" sheet.
  * It's also very well maintained, but the format is much harder to parse. It's more user-friendly than parser-friendly.
* [schau1's weaponData.csv](https://github.com/schau1/ff7ec/blob/main/weaponData.csv): This is the CSV used by <https://schau1.github.io/ff7ec/>.
  * It may take a few days to be updated, though the author accepts PRs.


## Decisions

* We do not consider Ultimate Weapons; they're usually not a reliably way of keeping buffs/debuffs applied throughout a battle.
* We consider "Healing Weapons" as weapons that have either a `All (Cure Spells)` S. Ability or a C. Ability that heals for at least 35%.
    * This last criterium is used to exclude weapons like Microlaser and Flower Vase, but include Lifeguard Wraps (38% at OB0).
* We don't discard teams that are superset of other teams.
    * Say you filter by `PATK Up All + MATK Up All`. Among the possible teams, you'll find these:
        1. Cait Sith with X
        1. Cait Sith with X + Barret with Y
    * At first it might seem like the 2nd team is redundant: if Cait Sith can do it all with one
    weapon, why force Barret into your team?
      However, Cait's weapon may have high potency for `PATK Up` and low potency for `MATK Up`, whereas Barret's might have high `MATK Up`, in which case it makes sense to have both.
* When sorting the possible teams, the highest criterium is to have as high potency as possible.
  Fitting many effects into fewer characters is an important criterium, but not as important as having high potency.
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
