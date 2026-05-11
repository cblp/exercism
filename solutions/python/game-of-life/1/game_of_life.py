# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from copy import deepcopy


def neighbours(matrix: list[list[int]], i: int, j: int) -> int:
    n = 0

    if i >= 1:
        if j >= 1:
            n += matrix[i - 1][j - 1]
        n += matrix[i - 1][j]
        if j + 1 < len(matrix[i - 1]):
            n += matrix[i - 1][j + 1]

    if j >= 1:
        n += matrix[i][j - 1]
    if j + 1 < len(matrix[i]):
        n += matrix[i][j + 1]

    if i + 1 < len(matrix):
        if j >= 1:
            n += matrix[i + 1][j - 1]
        n += matrix[i + 1][j]
        if j + 1 < len(matrix[i + 1]):
            n += matrix[i + 1][j + 1]

    return n


def tick(matrix: list[list[int]]) -> list[list[int]]:
    result = deepcopy(matrix)
    for i, row in enumerate(matrix):
        for j in range(len(row)):
            n = neighbours(matrix, i, j)
            if n < 2:
                result[i][j] = 0
            elif n == 3:
                result[i][j] = 1
            elif n > 3:
                result[i][j] = 0
    return result
