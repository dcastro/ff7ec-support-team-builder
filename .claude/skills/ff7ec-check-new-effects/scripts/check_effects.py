"""
Check resources/weapons.json for weapon effects not yet handled by the parser.

Run from the project root:
  python .claude/skills/ff7ec-check-new-effects/scripts/check_effects.py
"""

import json
import re
import sys
from collections import defaultdict
from pathlib import Path

project_root = Path(__file__).parent.parent.parent.parent.parent
weapons_path = project_root / 'resources' / 'weapons.json'

with open(weapons_path) as f:
    data = json.load(f)

rows = data['values']

# Columns 18-21 (0-indexed) = OB0, OB1, OB6, OB10
OB_COLS = [18, 19, 20, 21]
OB_LABELS = {18: 'OB0', 19: 'OB1', 20: 'OB6', 21: 'OB10'}

# Keywords from Parser.purs parseWeaponEffect — update this list when the parser gains new effects
KNOWN_KEYWORDS = {
    'Heal',
    'PATK Up', 'MATK Up', 'PDEF Up', 'MDEF Up',
    'Fire Damage Up', 'Ice Damage Up', 'Thunder Damage Up',
    'Earth Damage Up', 'Water Damage Up', 'Wind Damage Up',
    'Veil',
    'Provoke',
    'PATK Down', 'MATK Down', 'PDEF Down', 'MDEF Down',
    'Fire Resistance Down', 'Ice Resistance Down', 'Thunder Resistance Down',
    'Earth Resistance Down', 'Water Resistance Down', 'Wind Resistance Down',
    'Fire Weakness', 'Ice Weakness', 'Thunder Weakness',
    'Earth Weakness', 'Water Weakness', 'Wind Weakness',
    'Enfeeble',
    'Stop',
    'Enliven',
    'WeaknessAttackUp',
    'Exploit Weakness',
}


def extract_keyword(line):
    """Strip numeric prefix (duration/percentage) and trailing parameters/range to get the effect keyword."""
    s = line.strip()
    # Strip bracketed tags at the end: [Range: ...] [Condition: ...]
    s = re.sub(r'\s*\[.*?\]', '', s).strip()
    # Strip leading duration "Ns "
    s = re.sub(r'^\d+s\s+', '', s)
    # Strip leading percentage "N% "
    s = re.sub(r'^\d+%\s+', '', s)
    # Strip leading duration again (for "Ns N% Keyword" pattern like Veil/ExploitWeakness)
    s = re.sub(r'^\d+s\s+', '', s)
    # Take everything up to the first ( which starts extension/potency parameters
    keyword = re.split(r'\s*\(', s)[0].strip()
    return keyword


def normalize_keyword(keyword):
    """Replace all numeric values (int and float) with N for grouping similar effect types."""
    return re.sub(r'\d+\.?\d*', 'N', keyword)


def is_ability_name_line(line):
    """First line of each OB cell: 'AbilityName [Sigils: ...] (N ATB)' — not an effect."""
    return bool(re.search(r'\(\d+ ATB\)', line))


def is_atb_bonus(line):
    """Lines like '+1 ATB [Range: Other Allies] [Condition: ...]' — not a weapon effect."""
    return bool(re.match(r'^\+\d+ ATB', line.strip()))


# Map: normalized_keyword -> set of "CharName: WeaponName" strings
unknown: defaultdict[str, set] = defaultdict(set)

for row in rows[1:]:
    if len(row) < 2:
        continue
    weapon_name = (row[0] or '').strip()
    char_name = (row[1] or '').strip() if len(row) > 1 else ''
    label = f"{char_name}: {weapon_name}" if char_name else weapon_name

    for col in OB_COLS:
        if col >= len(row) or not row[col]:
            continue
        lines = re.split(r'\r?\n', row[col])
        for i, line in enumerate(lines):
            line = line.strip()
            if not line:
                continue
            if i == 0 or is_ability_name_line(line):
                continue
            if is_atb_bonus(line):
                continue
            keyword = extract_keyword(line)
            if keyword and keyword not in KNOWN_KEYWORDS:
                normalized = normalize_keyword(keyword)
                unknown[normalized].add(label)

if not unknown:
    print("All weapon effects in weapons.json are supported by the parser.")
    sys.exit(0)

print(f"Found {len(unknown)} unsupported effect type(s):\n")
col_w = max(len(k) for k in unknown) + 2

header = f"{'Effect type':<{col_w}}  Weapons"
print(header)
print('-' * len(header))

for keyword in sorted(unknown.keys()):
    weapons = sorted(unknown[keyword])
    first = weapons[0]
    print(f"{keyword:<{col_w}}  {first}")
    for w in weapons[1:]:
        print(f"{'':<{col_w}}  {w}")
