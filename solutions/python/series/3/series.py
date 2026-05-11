"""
Utilities for extracting contiguous substrings ("slices") from a string.

This module provides the function
`slices(series: str, length: int) -> list[str]`,
which returns all consecutive substrings of the given `series` that are exactly
`length` characters long.
Substrings are returned in the order they appear in `series`.

Behavior and error conditions
- If `series` is an empty string, a ValueError is raised.
- If `length` is greater than the length of `series`, a ValueError is raised.
- If `length` is zero or negative, a ValueError is raised.

Example
>>> slices("01234", 2)
["01", "12", "23", "34"]

The function is intended for simple slicing of strings and performs no
validation beyond the length-related checks described above.

"""


def slices(series: str, length: int) -> list[str]:
    if not series:
        raise ValueError("series cannot be empty")
    if length > len(series):
        raise ValueError("slice length cannot be greater than series length")
    if length == 0:
        raise ValueError("slice length cannot be zero")
    if length < 0:
        raise ValueError("slice length cannot be negative")
    return [
        series[start : start + length]
        for start in range(0, len(series) - length + 1)
    ]
