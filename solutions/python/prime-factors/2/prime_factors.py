from itertools import chain, count


def factors(value: int) -> list[int]:
    result = []
    for x in chain([2], count(3, 2)):
        if value == 1:
            break
        while value % x == 0:
            result.append(x)
            value //= x
    return result
