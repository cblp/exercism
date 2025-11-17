"""
This exercise stub and the test suite contain several enumerated constants.

Enumerated constants can be done with a NAME assigned to an arbitrary,
but unique value. An integer is traditionally used because it’s memory
efficient.
It is a common practice to export both constants and functions that work with
those constants (ex. the constants in the os, subprocess and re modules).

You can learn more here: https://en.wikipedia.org/wiki/Enumerated_type
"""

from enum import Enum, auto
from typing import TypeVar

A = TypeVar("A")


class ListRelation(Enum):
    SUBLIST = auto()
    SUPERLIST = auto()
    EQUAL = auto()
    UNEQUAL = auto()


SUBLIST = ListRelation.SUBLIST
SUPERLIST = ListRelation.SUPERLIST
EQUAL = ListRelation.EQUAL
UNEQUAL = ListRelation.UNEQUAL


def is_sublist(list_smaller: list[A], list_bigger: list[A]) -> bool:
    return any(
        list_bigger[i : i + len(list_smaller)] == list_smaller
        for i in range(len(list_bigger) - len(list_smaller) + 1)
    )


def sublist(list_one: list[A], list_two: list[A]) -> ListRelation:
    if len(list_one) == len(list_two):
        return EQUAL if list_one == list_two else UNEQUAL
    if len(list_one) < len(list_two):
        return SUBLIST if is_sublist(list_one, list_two) else UNEQUAL
    return SUPERLIST if is_sublist(list_two, list_one) else UNEQUAL
