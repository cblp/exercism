# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


Result = tuple[int, list[tuple[int, int]]]
EmptyResult = tuple[None, list[tuple[int, ...]]]


def largest(min_factor: int, max_factor: int) -> Result | EmptyResult:
    """Given a range of numbers, find the largest palindromes which
    are products of two numbers within that range.
    """

    return _palindrome(min_factor, max_factor, True)


def smallest(min_factor: int, max_factor: int) -> Result | EmptyResult:
    """Given a range of numbers, find the smallest palindromes which
    are products of two numbers within that range.
    """

    return _palindrome(min_factor, max_factor, False)


def _palindrome(
    min_factor: int, max_factor: int, chooser: bool
) -> Result | EmptyResult:
    if min_factor > max_factor:
        raise ValueError("min must be <= max")
    value: int | None = None
    factors: list[tuple[int, int]] = []
    for a in range(min_factor, max_factor + 1):
        if value is not None and a * a > value:
            break
        for b in range(a, max_factor + 1):
            v = a * b
            if _is_palingrome(v):
                if value is None:
                    value = v
                    factors = [(a, b)]
                else:
                    value, factors = _choose(chooser, (value, factors), v, a, b)
    if value is None:
        return (None, [])
    return value, factors


def _choose(chooser: bool, p1: Result, value2: int, a2: int, b2: int) -> Result:
    value1, factors1 = p1
    p2 = (value2, [(a2, b2)])
    if value1 is None:
        return p2
    if value1 == value2:
        return (value1, factors1 + [(a2, b2)])
    if value1 > value2 if chooser else value1 < value2:
        return p1
    return p2


def _is_palingrome(value: int) -> bool:
    s = str(value)
    return s == s[::-1]
