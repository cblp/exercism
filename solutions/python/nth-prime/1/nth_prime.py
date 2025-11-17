from itertools import count

PRIMES = [2, 3]


def prime(n: int) -> int:
    if n < 1:
        raise ValueError("there is no zeroth prime")
    while n > len(PRIMES):
        for x in count(PRIMES[-1] + 2, 2):
            if all(x % p for p in PRIMES):
                PRIMES.append(x)
                break
    return PRIMES[n - 1]
