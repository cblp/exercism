# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from typing import Optional
from itertools import cycle
import secrets
from string import ascii_lowercase


class Cipher:
    key: str

    def __init__(self, key: Optional[str] = None):
        self.key = key or "".join(
            secrets.choice(ascii_lowercase) for _ in range(100)
        )

    def encode(self, text: str) -> str:
        return "".join(
            chr((ord(t) + ord(k) - 194) % 26 + 97)
            for t, k in zip(text, cycle(self.key))
        )

    def decode(self, text: str) -> str:
        return "".join(
            chr((ord(t) - ord(k)) % 26 + 97)
            for t, k in zip(text, cycle(self.key))
        )
