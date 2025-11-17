def decode(base: int, digits: list[int]) -> int:
    if base < 2:
        raise ValueError("input base must be >= 2")
    number = 0
    for digit in digits:
        if digit < 0 or base <= digit:
            raise ValueError("all digits must satisfy 0 <= d < input base")
        number = number * base + digit
    return number


def encode(base: int, number: int) -> list[int]:
    if base < 2:
        raise ValueError("output base must be >= 2")
    digits = []
    while number:
        digit = number % base
        number = number // base
        digits.append(digit)
    return list(reversed(digits)) if digits else [0]


def rebase(input_base: int, digits: list[int], output_base: int) -> list[int]:
    return encode(output_base, decode(input_base, digits))
