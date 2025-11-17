from collections import deque
from itertools import islice
from math import prod
from typing import Iterable, TypeVar

A = TypeVar("A")


def sliding_window(iterable: Iterable[A], n: int) -> Iterable[tuple[A, ...]]:
    "Collect data into overlapping fixed-length chunks or blocks."
    # sliding_window('ABCDEFG', 4) → ABCD BCDE CDEF DEFG
    iterator = iter(iterable)
    window = deque(islice(iterator, n - 1), maxlen=n)
    for x in iterator:
        window.append(x)
        yield tuple(window)


def assert_decimal(c: str) -> int:
    try:
        return int(c)
    except ValueError as e:
        raise ValueError("digits input must only contain digits") from e


def largest_product(series: str, size: int) -> int:
    if size < 0:
        raise ValueError("span must not be negative")
    if size > len(series):
        raise ValueError("span must not exceed string length")
    return max(
        prod(window)
        for window in sliding_window((assert_decimal(c) for c in series), size)
    )
