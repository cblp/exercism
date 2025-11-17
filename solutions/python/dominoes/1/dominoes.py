from typing import Optional, Iterator


Domino = tuple[int, int]
Dominoes = list[Domino]


def get_next_steps(
    free_dominoes: Dominoes, start: int
) -> Iterator[tuple[Domino, Dominoes]]:
    for d in free_dominoes:
        a, b = d
        rest = free_dominoes[:]
        rest.remove(d)
        if a == start:
            yield (d, rest)
        if b == start and b != a:
            yield ((b, a), rest)


def can_chain_from_to(
    free_dominoes: Dominoes, start: int, end: int
) -> Optional[Dominoes]:
    print(free_dominoes, start, end)
    if not free_dominoes:
        return [] if start == end else None
    next_steps = list(get_next_steps(free_dominoes, start))
    if not next_steps:
        return None
    for next_domino, rest_free_dominoes in next_steps:
        next_chain = can_chain_from_to(rest_free_dominoes, next_domino[1], end)
        if next_chain is not None:
            return [next_domino] + next_chain
    return None


def can_chain(dominoes: Dominoes) -> Optional[Dominoes]:
    if not dominoes:
        return []
    first = dominoes[0]
    r = can_chain_from_to(dominoes[1:], first[1], first[0])
    return None if r is None else [first] + r
