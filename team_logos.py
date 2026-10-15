from __future__ import annotations
import difflib
import re

TEAM_ESPN_ID: dict[str, int] = {
    "Alabama": 333, "Arkansas": 8, "Auburn": 2, "Florida": 57, "Georgia": 61,
    "Kentucky": 96, "LSU": 99, "Mississippi State": 344, "Missouri": 142,
    "Ole Miss": 145, "Oklahoma": 201, "South Carolina": 2579,
    "Tennessee": 2633, "Texas": 251, "Texas A&M": 245, "Vanderbilt": 238,

    "Illinois": 356, "Indiana": 84, "Iowa": 2294, "Maryland": 120,
    "Michigan": 130, "Michigan State": 127, "Minnesota": 135,
    "Nebraska": 158, "Northwestern": 77, "Ohio State": 194, "Oregon": 2483,
    "Penn State": 213, "Purdue": 2509, "Rutgers": 164, "UCLA": 26,
    "USC": 30, "Washington": 264, "Wisconsin": 275,

    "Boston College": 103, "California": 25, "Clemson": 228, "Duke": 150,
    "Florida State": 52, "Georgia Tech": 59, "Louisville": 97,
    "Miami (FL)": 2390, "NC State": 152, "North Carolina": 153,
    "Notre Dame": 87, "Pittsburgh": 221, "SMU": 2567, "Stanford": 24,
    "Syracuse": 183, "Virginia": 258, "Virginia Tech": 259,
    "Wake Forest": 154,

    "Arizona": 12, "Arizona State": 9, "Baylor": 239, "BYU": 252,
    "Cincinnati": 2132, "Colorado": 38, "Houston": 248, "Iowa State": 66,
    "Kansas": 2305, "Kansas State": 2306, "Oklahoma State": 197,
    "TCU": 2628, "Texas Tech": 2641, "UCF": 2116, "Utah": 254,
    "West Virginia": 277,

    "Charlotte": 2429, "East Carolina": 151, "Florida Atlantic": 2226,
    "Memphis": 235, "Navy": 2426, "North Texas": 249, "Rice": 242,
    "South Florida": 58, "Temple": 218, "Tulane": 2655, "Tulsa": 202,
    "UAB": 5, "UTSA": 2636, "Army": 349,

    "Air Force": 2005, "Boise State": 68, "Colorado State": 36,
    "Fresno State": 278, "Hawai'i": 62, "Nevada": 2440, "New Mexico": 167,
    "San Diego State": 21, "San Jose State": 23, "UNLV": 2439,
    "Utah State": 328, "Wyoming": 2751, "Washington State": 265,
    "Oregon State": 204,

    "FIU": 2229, "Jacksonville State": 55, "Kennesaw State": 338,
    "Liberty": 2335, "Louisiana Tech": 2348, "Middle Tennessee": 2393,
    "New Mexico State": 166, "Sam Houston": 2534, "UTEP": 2638,
    "Western Kentucky": 98,

    "Akron": 2006, "Ball State": 2050, "Bowling Green": 189,
    "Buffalo": 2084, "Central Michigan": 2117, "Eastern Michigan": 2199,
    "Kent State": 2309, "Miami (OH)": 193, "Northern Illinois": 2459,
    "Ohio": 195, "Toledo": 2649, "Western Michigan": 2711,
    "UMass": 113,

    "Appalachian State": 2026, "Arkansas State": 2032,
    "Coastal Carolina": 324, "Georgia Southern": 290,
    "Georgia State": 2247, "James Madison": 256, "Louisiana": 309,
    "Louisiana-Monroe": 2433, "Marshall": 276, "Old Dominion": 295,
    "South Alabama": 6, "Southern Miss": 2572, "Texas State": 326,
    "Troy": 2653,

    "UConn": 41,

    "Delaware": 48, "Missouri State": 2623,
    "North Dakota State": 2449, "Sacramento State": 16,
}

SKIP_NAMES = {"bye", "tbd", "n/a"}

TEAM_ALIASES: dict[str, str] = {
    "mississippi": "Ole Miss",
    "miami": "Miami (FL)",
    "miami fl": "Miami (FL)",
    "miami oh": "Miami (OH)",
    "pitt": "Pittsburgh",
    "cal": "California",
    "southern california": "USC",
    "central florida": "UCF",
    "smu": "SMU",
    "nc state": "NC State",
    "north carolina state": "NC State",
    "texas am": "Texas A&M",
    "louisiana lafayette": "Louisiana",
    "louisiana-lafayette": "Louisiana",
    "ul monroe": "Louisiana-Monroe",
    "ulm": "Louisiana-Monroe",
    "la monroe": "Louisiana-Monroe",
    "connecticut": "UConn",
    "massachusetts": "UMass",
    "app state": "Appalachian State",
    "hawaii": "Hawai'i",
    "san jose st": "San Jose State",
    "ole miss rebels": "Ole Miss",
    "florida international": "FIU",
    "fiu panthers": "FIU",
}

LOGO_URL_TEMPLATE = "https://a.espncdn.com/i/teamlogos/ncaa/500/{id}.png"

BLANK_LOGO = "https://a.espncdn.com/i/teamlogos/ncaa/500/default-team-logo-500.png"


def _normalize(name: str) -> str:
    name = name.strip().lower()
    name = name.replace("&", "and")
    name = re.sub(r"[().]", "", name)
    name = re.sub(r"\s+", " ", name)
    return name.strip()


_NORMALIZED_LOOKUP = {_normalize(k): k for k in TEAM_ESPN_ID}


def get_logo_url(team_name: str) -> str | None:
    if not team_name:
        return None

    norm = _normalize(team_name)

    if norm in _NORMALIZED_LOOKUP:
        return LOGO_URL_TEMPLATE.format(id=TEAM_ESPN_ID[_NORMALIZED_LOOKUP[norm]])

    if norm in TEAM_ALIASES:
        canonical = TEAM_ALIASES[norm]
        return LOGO_URL_TEMPLATE.format(id=TEAM_ESPN_ID[canonical])

    close = difflib.get_close_matches(norm, _NORMALIZED_LOOKUP.keys(), n=1, cutoff=0.85)
    if close:
        canonical = _NORMALIZED_LOOKUP[close[0]]
        return LOGO_URL_TEMPLATE.format(id=TEAM_ESPN_ID[canonical])

    return None


def missing_teams(team_names) -> list[str]:
    seen = set()
    missing = []
    for name in team_names:
        if name in seen:
            continue
        seen.add(name)
        if _normalize(str(name)) in SKIP_NAMES:
            continue
        if get_logo_url(name) is None:
            missing.append(name)
    return sorted(missing)