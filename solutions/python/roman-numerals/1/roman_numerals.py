from typing import Callable

ONE = ["I", "X", "C", "M"]
FIVE = ["V", "L", "D"]


def div_mod(a: int, b: int) -> tuple[int, int]:
    return (a // b, a % b)


def roman_digit(m: int, digit: str) -> str:
    digits_combined: dict[str, Callable[[], str]] = {
        "0": lambda: "",
        "1": lambda: ONE[m],
        "2": lambda: ONE[m] + ONE[m],
        "3": lambda: ONE[m] + ONE[m] + ONE[m],
        "4": lambda: ONE[m] + FIVE[m],
        "5": lambda: FIVE[m],
        "6": lambda: FIVE[m] + ONE[m],
        "7": lambda: FIVE[m] + ONE[m] + ONE[m],
        "8": lambda: FIVE[m] + ONE[m] + ONE[m] + ONE[m],
        "9": lambda: ONE[m] + ONE[m + 1],
    }
    return digits_combined[digit]()


def roman(number: int) -> str:
    return "".join(
        reversed(
            [
                roman_digit(magnitude, digit)
                for magnitude, digit in enumerate(reversed(str(number)))
            ]
        )
    )
