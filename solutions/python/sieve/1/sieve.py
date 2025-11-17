def primes(limit: int) -> list[int]:
    known_primes: list[int] = []
    x = 2
    while x <= limit:
        if all(x % p for p in known_primes):
            known_primes.append(x)
        x += 1
    return known_primes
