NUMBERS = [
    "no",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
]


def suffix(i: int) -> str:
    return "" if i == 1 else "s"


def verse(i: int) -> list[str]:
    current_titlecased = NUMBERS[i].title()
    current_suffix = suffix(i)
    next_cardinal = NUMBERS[i - 1]
    next_suffix = suffix(i - 1)
    return [
        f"{current_titlecased} green bottle{current_suffix} "
        "hanging on the wall,"
    ] * 2 + [
        "And if one green bottle should accidentally fall,",
        f"There'll be {next_cardinal} green bottle{next_suffix} "
        "hanging on the wall.",
    ]


def recite(start: int, take: int = 1) -> list[str]:
    result = []
    for i in range(start, start - take, -1):
        if i != start:
            result.append("")
        result.extend(verse(i))
    return result
