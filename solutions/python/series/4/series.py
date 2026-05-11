"""Utilities for extracting contiguous substrings ("slices") from a string."""


def slices(series: str, length: int) -> list[str]:
    """Return all contiguous substrings (slices) of a given length from a digit
    series.
    Given a string `series`, this function extracts every contiguous substring
    of length `length` and returns them in the order they appear. Each slice is
    returned as a string.

    Parameters
    ----------
    series : str
        The input string from which to extract slices.
    length : int
        The desired length of each slice.

    Returns
    -------
    list[str]
        A list of substrings of length `length`.

    Raises
    ------
    ValueError
        If `series` is empty.
        If `length` is greater than the length of `series`.
        If `length` is zero or negative.

    Examples
    --------
    >>> slices("01234", 3)
    ['012', '123', '234']
    >>> slices("820", 2)
    ['82', '20']
    """

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
