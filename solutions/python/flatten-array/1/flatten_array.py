from typing import Iterable, Any


def flatten(iterable: Iterable[Any]) -> list[Any]:
    result = []
    for x in iterable:
        if x is None:
            continue
        if isinstance(x, list):
            result.extend(flatten(x))
        else:
            result.append(x)
    return result
