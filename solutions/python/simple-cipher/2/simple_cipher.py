# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from typing import Optional
from itertools import cycle
import secrets
from string import ascii_lowercase

ORD_A = ord("a")


def letter_to_int(c: str) -> int:
    return ord(c) - ORD_A


def int_to_letter(i: int) -> str:
    return chr(i + ORD_A)


class Cipher:
    key: str

    def __init__(self, key: Optional[str] = None):
        self.key = key or "".join(
            secrets.choice(ascii_lowercase) for _ in range(100)
        )

    def encode(self, text: str) -> str:
        return "".join(
            int_to_letter((letter_to_int(t) + letter_to_int(k)) % 26)
            for t, k in zip(text, cycle(self.key))
        )

    def decode(self, text: str) -> str:
        return "".join(
            int_to_letter((letter_to_int(t) - letter_to_int(k)) % 26)
            for t, k in zip(text, cycle(self.key))
        )
