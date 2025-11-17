from itertools import zip_longest
from typing import Optional, TypeVar, Iterable

A = TypeVar("A")


def rstrip(it: Iterable[Optional[A]]) -> list[Optional[A]]:
    ls = list(it)
    while ls and ls[-1] is None:
        ls.pop()
    return ls


def transpose(text: str) -> str:
    return "\n".join(
        "".join(c or " " for c in rstrip(col))
        for col in zip_longest(*text.splitlines())
    )
