def collatz(n: int) -> int:
    return n * 3 + 1 if n % 2 else n // 2

def steps(number: int):
    if number <= 0:
        raise ValueError("Only positive integers are allowed")

    r = 0
    while number != 1:
        number = collatz(number)
        r += 1
    return r
