CHARACTERS = [
    "house that Jack built",
    "malt",
    "rat",
    "cat",
    "dog",
    "cow with the crumpled horn",
    "maiden all forlorn",
    "man all tattered and torn",
    "priest all shaven and shorn",
    "rooster that crowed in the morn",
    "farmer sowing his corn",
    "horse and the hound and the horn",
]

RELATIONS = [
    "belonged to",
    "kept",
    "woke",
    "married",
    "kissed",
    "milked",
    "tossed",
    "worried",
    "killed",
    "ate",
    "lay in",
]

MIDDLE_LINES = [
    f"{relation} the {character}"
    for relation, character in zip(RELATIONS, reversed(CHARACTERS[:-1]))
]


VERSES = [
    f"This is the {body}."
    for body in [
        " that ".join([CHARACTERS[n]] + MIDDLE_LINES[11 - n :])
        for n in range(12)
    ]
]


def recite(start_verse: int, end_verse: int) -> list[str]:
    return VERSES[start_verse - 1 : end_verse]
