from typing import TypedDict


# type Point = dict[str, int]
class Point(TypedDict):
    row: int
    column: int


def find_max_indexes(row: list[int]) -> list[int]:
    if not row:
        return []
    res = [0]
    for j in range(1, len(row)):
        a = row[res[0]]
        b = row[j]
        if a == b:
            res.append(j)
        elif a < b:
            res = [j]
    return res


def is_min_in_column(matrix: list[list[int]], i: int, j: int) -> bool:
    value = matrix[i][j]
    return all(row[j] >= value for row in matrix)


def saddle_points(matrix: list[list[int]]) -> list[Point]:
    if not matrix:
        return []
    width = len(matrix[0])
    if not all(len(row) == width for row in matrix):
        raise ValueError("irregular matrix")
    return [
        Point(row=i + 1, column=j + 1)
        for i, row in enumerate(matrix)
        for j in find_max_indexes(row)
        if is_min_in_column(matrix, i, j)
    ]
