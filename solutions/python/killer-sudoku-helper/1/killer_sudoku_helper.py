# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from itertools import islice


def combinations(target: int, size: int, exclude: list[int]) -> list[list[int]]:
    min_value = 1
    max_value = target - sum(
        islice((v for v in range(1, target + 1) if v not in exclude), size - 1)
    )
    free_values = {
        v for v in range(min_value, max_value + 1) if v not in exclude
    }
    return sorted(
        list(combination)
        for combination in _combinations(target, size, free_values)
    )


def _combinations(
    target: int, size: int, free_values: set[int]
) -> set[tuple[int, ...]]:
    print(target, size, free_values)
    if sum(free_values) == target and len(free_values) == size:
        return {tuple(sorted(free_values))}
    if (
        sum(free_values) < target
        or len(free_values) < size
        or size <= 0
        or target <= 0
    ):
        return set()
    if size == 1 and target in free_values:
        return {(target,)}
    return {
        tuple(sorted((v,) + combination))
        for v in free_values
        for combination in _combinations(
            target - v, size - 1, free_values - {v}
        )
    }
