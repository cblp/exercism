R = 0
D = 1
L = 2
U = 3


def spiral_matrix(size: int) -> list[list[int]]:
    matrix = [[0] * size for _ in range(size)]
    i, j = 0, 0
    direction = R
    for n in range(1, size * size + 1):
        matrix[i][j] = n
        if direction == R:
            next_i, next_j = i, j + 1
            if next_j >= size or matrix[next_i][next_j]:
                direction = D
                next_i, next_j = i + 1, j
        elif direction == D:
            next_i, next_j = i + 1, j
            if next_i >= size or matrix[next_i][next_j]:
                direction = L
                next_i, next_j = i, j - 1
        elif direction == L:
            next_i, next_j = i, j - 1
            if next_j < 0 or matrix[next_i][next_j]:
                direction = U
                next_i, next_j = i - 1, j
        else:  # direction == U
            next_i, next_j = i - 1, j
            if next_i < 0 or matrix[next_i][next_j]:
                direction = R
                next_i, next_j = i, j + 1
        i, j = next_i, next_j
    return matrix
