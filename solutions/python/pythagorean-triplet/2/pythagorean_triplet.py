def triplets_with_sum(number: int) -> list[list[int]]:
    return [
        [a, b, c]
        for a in range(1, number // 3)
        for b in range(a + 1, min(number - a, 2 * number // 3))
        for c in [number - a - b]
        if c > b and a * a + b * b == c * c
    ]
