from itertools import chain


def row(size: int, step: int) -> str:
    letter = chr(ord("A") + step)
    return (
        " " * (size - step)
        + (letter if step == 0 else letter + " " * (2 * step - 1) + letter)
        + " " * (size - step)
    )


def rows(letter: str) -> list[str]:
    size = ord(letter) - ord("A")
    return [
        row(size, i) for i in chain(range(size + 1), range(size - 1, -1, -1))
    ]
