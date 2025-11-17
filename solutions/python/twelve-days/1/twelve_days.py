GIFTS = [
    "a Partridge in a Pear Tree",
    "two Turtle Doves",
    "three French Hens",
    "four Calling Birds",
    "five Gold Rings",
    "six Geese-a-Laying",
    "seven Swans-a-Swimming",
    "eight Maids-a-Milking",
    "nine Ladies Dancing",
    "ten Lords-a-Leaping",
    "eleven Pipers Piping",
    "twelve Drummers Drumming",
]

ORDINALS = ["first", "second", "third"] + [
    word + "th"
    for word in [
        "four",
        "fif",
        "six",
        "seven",
        "eigh",
        "nin",
        "ten",
        "eleven",
        "twelf",
    ]
]


def oxford_join(parts: list[str]) -> str:
    if len(parts) == 1:
        return parts[0]
    return ", ".join(parts[:-1] + ["and " + parts[-1]])


VERSES = [
    f"On the {ORDINALS[day - 1]} day of Christmas my true love gave to me: "
    f"{oxford_join(GIFTS[day - 1 : : -1])}."
    for day in range(1, 13)
]


def recite(start_verse: int, end_verse: int) -> list[str]:
    return VERSES[start_verse - 1 : end_verse]
