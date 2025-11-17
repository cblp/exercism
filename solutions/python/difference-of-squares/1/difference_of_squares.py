def square(n: int) -> int:
    return n * n


def square_of_sum(n: int) -> int:
    return square(sum(range(n + 1)))


def sum_of_squares(n: int) -> int:
    return sum(square(x) for x in range(n + 1))


def difference_of_squares(n: int) -> int:
    return square_of_sum(n) - sum_of_squares(n)
