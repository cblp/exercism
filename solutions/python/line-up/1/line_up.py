"""Utilities for greeting customers with their ordinal position in line."""


def ending(number: int) -> str:
    """Return the ordinal suffix ('st', 'nd', 'rd', or 'th') for a positive
    integer."""
    if number % 100 // 10 == 1:
        return "th"
    return {1: "st", 2: "nd", 3: "rd"}.get(number % 10, "th")


def line_up(name: str, number: int) -> str:
    """Return a personalized greeting for a customer at the given position in
    line."""
    end = ending(number)
    return (
        f"{name}, you are the {number}{end} customer we serve today. Thank you!"
    )
