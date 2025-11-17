from collections import Counter
from typing import Iterable

COST_1 = 8_00
COST_2 = 15_20
COST_3 = 21_60
COST_4 = 25_60
COST_5 = 30_00


"""
[1, 1, 2, 3, 4, 4, 5, 5]
-> diagram
 1     4 5
 1 2 3 4 5
[2,1,1,2,2]
-> normalize
(2,2,2,1,1)

        0   1   2   3   4   5
1 ->    1   1
2 ->    1   2   1
3 ->    1   3   3   1
4 ->    1   4   6   4   1
5 ->    1   5  10  10   5   1
"""


BasketNormalized = tuple[int, ...]


def chop(basket: Iterable[int]) -> BasketNormalized:
    return tuple(filter(None, basket))


def normalize(basket: Iterable[int]) -> BasketNormalized:
    return chop(sorted(basket, reverse=True))


def take_some(n: int, basket: BasketNormalized) -> list[BasketNormalized]:
    if n > len(basket):
        return []

    if n == len(basket):
        assert all(x >= 1 for x in basket)
        return [chop(tuple(x - 1 for x in basket))]

    # n < len(basket)
    basket_variants: list[tuple[int, ...]]
    match n, basket:
        case 2, (b1, b2, b3):
            basket_variants = [
                (b1 - 1, b2 - 1, b3),
                (b1 - 1, b2, b3 - 1),
                (b1, b2 - 1, b3 - 1),
            ]
        case 2, (b1, b2, b3, b4):
            basket_variants = [
                (b1 - 1, b2 - 1, b3, b4),
                (b1 - 1, b2, b3 - 1, b4),
                (b1 - 1, b2, b3, b4 - 1),
                (b1, b2 - 1, b3 - 1, b4),
                (b1, b2 - 1, b3, b4 - 1),
                (b1, b2, b3 - 1, b4 - 1),
            ]
        case 3, (b1, b2, b3, b4):
            basket_variants = [
                (b1 - 1, b2 - 1, b3 - 1, b4),
                (b1 - 1, b2 - 1, b3, b4 - 1),
                (b1 - 1, b2, b3 - 1, b4 - 1),
                (b1, b2 - 1, b3 - 1, b4 - 1),
            ]
        case 2, (b1, b2, b3, b4, b5):
            basket_variants = [
                (b1 - 1, b2 - 1, b3, b4, b5),
                (b1 - 1, b2, b3 - 1, b4, b5),
                (b1 - 1, b2, b3, b4 - 1, b5),
                (b1 - 1, b2, b3, b4, b5 - 1),
                (b1, b2 - 1, b3 - 1, b4, b5),
                (b1, b2 - 1, b3, b4 - 1, b5),
                (b1, b2 - 1, b3, b4, b5 - 1),
                (b1, b2, b3 - 1, b4 - 1, b5),
                (b1, b2, b3 - 1, b4, b5 - 1),
                (b1, b2, b3, b4 - 1, b5 - 1),
            ]
        case 3, (b1, b2, b3, b4, b5):
            basket_variants = [
                (b1 - 1, b2 - 1, b3 - 1, b4, b5),
                (b1 - 1, b2 - 1, b3, b4 - 1, b5),
                (b1 - 1, b2 - 1, b3, b4, b5 - 1),
                (b1 - 1, b2, b3 - 1, b4 - 1, b5),
                (b1 - 1, b2, b3 - 1, b4, b5 - 1),
                (b1 - 1, b2, b3, b4 - 1, b5 - 1),
                (b1, b2 - 1, b3 - 1, b4 - 1, b5),
                (b1, b2 - 1, b3 - 1, b4, b5 - 1),
                (b1, b2 - 1, b3, b4 - 1, b5 - 1),
                (b1, b2, b3 - 1, b4 - 1, b5 - 1),
            ]
        case 4, (b1, b2, b3, b4, b5):
            basket_variants = [
                (b1 - 1, b2 - 1, b3 - 1, b4 - 1, b5),
                (b1 - 1, b2 - 1, b3 - 1, b4, b5 - 1),
                (b1 - 1, b2 - 1, b3, b4 - 1, b5 - 1),
                (b1 - 1, b2, b3 - 1, b4 - 1, b5 - 1),
                (b1, b2 - 1, b3 - 1, b4 - 1, b5 - 1),
            ]
        case _:
            raise NotImplementedError(n, basket)

    return list(set(normalize(bv) for bv in basket_variants))


def total_impl(basket: BasketNormalized) -> int:
    if len(basket) == 0:
        return 0
    if len(basket) == 1:
        return COST_1 * basket[0]

    costs = []

    for without_5 in take_some(5, basket):
        costs.append(COST_5 + total_memoized(without_5))

    for without_4 in take_some(4, basket):
        costs.append(COST_4 + total_memoized(without_4))

    for without_3 in take_some(3, basket):
        costs.append(COST_3 + total_memoized(without_3))

    for without_2 in take_some(2, basket):
        costs.append(COST_2 + total_memoized(without_2))

    if costs:
        return min(costs)
    raise NotImplementedError(basket)


total_cache = {}


def total_memoized(basket: BasketNormalized) -> int:
    if basket not in total_cache:
        total_cache[basket] = total_impl(basket)
        print(total_cache)
    return total_cache[basket]


def total(basket: list[int]) -> int:
    basket_normalized = normalize(Counter(basket).values())
    return total_memoized(basket_normalized)
