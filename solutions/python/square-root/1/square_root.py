def square_root(n: int) -> int:
    """Integer square root"""

    x = 0
    while (x + 1) * (x + 1) <= n:
        x += 1
    return x
