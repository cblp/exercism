# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


from typing import cast
from functools import reduce

Result = tuple[int | None, frozenset[frozenset[int]]]


def largest(min_factor: int, max_factor: int) -> Result:
    """Given a range of numbers, find the largest palindromes which
    are products of two numbers within that range.
    """

    return _palindrome(min_factor, max_factor, True)


def smallest(min_factor: int, max_factor: int) -> Result:
    """Given a range of numbers, find the smallest palindromes which
    are products of two numbers within that range.
    """

    return _palindrome(min_factor, max_factor, False)


def _palindrome(min_factor: int, max_factor: int, chooser: bool) -> Result:
    if min_factor > max_factor:
        raise ValueError("min must be <= max")
    return reduce(
        lambda a, b: _choose(chooser, a, b),
        (
            (a * b, frozenset([frozenset([a, b])]))
            for a in range(min_factor, max_factor + 1)
            for b in range(a, max_factor + 1)
            if _is_palingrome(a * b)
        ),
        cast(Result, (None, frozenset())),
    )


def _choose(chooser: bool, p1: Result, p2: Result) -> Result:
    value1, factors1 = p1
    value2, factors2 = p2
    if value1 is None:
        return p2
    if value2 is None:
        raise RuntimeError("impossible")
    if value1 == value2:
        return (value1, factors1 | factors2)
    if value1 > value2 if chooser else value1 < value2:
        return p1
    return p2


def _is_palingrome(value: int) -> bool:
    s = str(value)
    return s[::-1] == s
