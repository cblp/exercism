primes = [2, 3]


def factors(value: int) -> list[int]:
    result = []
    for p in primes:
        if p > value:
            break
        while value % p == 0:
            result.append(p)
            value //= p
    x = primes[-1] + 2
    while value != 1:
        if all(x % p for p in primes):
            while value % x == 0:
                result.append(x)
                value //= x
        x += 2
    return result
