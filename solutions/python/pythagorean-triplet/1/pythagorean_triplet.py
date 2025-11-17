def triplets_with_sum(number: int) -> list[list[int]]:
    return [
        [a, b, c]
        for a in range(1, number // 2)
        for b in range(a + 1, number - a)
        for c in [number - a - b]
        if c > 0 and a * a + b * b == c * c
    ]
