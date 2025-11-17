from typing import Iterable, TypeVar


A = TypeVar("A")


def set_union(sets: Iterable[set[A]]) -> set[A]:
    return set().union(*sets)


def sum_of_multiples(limit: int, multiples: list[int]) -> int:
    return sum(set_union(set(range(m, limit, m)) for m in multiples if m > 0))
