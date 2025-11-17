from typing import Iterator


def to_digits(n: int) -> Iterator[int]:
    assert n > 0
    while n:
        yield n % 10
        n //= 10


ONES = [
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
]

TENS = [
    "ten",
    "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety",
]

TEENS = [
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen",
]


def say_group(digits: list[int]) -> Iterator[str]:
    digits.extend([0] * (3 - len(digits)))
    if digits[0]:
        if digits[1] == 1:
            yield TEENS[digits[0] - 1]
        elif digits[1]:
            yield TENS[digits[1] - 1] + "-" + ONES[digits[0] - 1]
        else:
            yield ONES[digits[0] - 1]
    else:
        if digits[1]:
            yield TENS[digits[1] - 1]
    if digits[2]:
        yield "hundred"
        yield ONES[digits[2] - 1]


def say_parts(digits: list[int]) -> Iterator[str]:
    yield from say_group(digits[:3])
    if len(digits) > 3 and any(digits[3:6]):
        yield "thousand"
        yield from say_group(digits[3:6])
    if len(digits) > 6 and any(digits[6:9]):
        yield "million"
        yield from say_group(digits[6:9])
    if len(digits) > 9:
        yield "billion"
        yield from say_group(digits[9:12])
    if len(digits) > 12:
        raise ValueError("input out of range")


def say(number: int) -> str:
    if number < 0:
        raise ValueError("input out of range")
    if number == 0:
        return "zero"
    return " ".join(reversed(list(say_parts(list(to_digits(number))))))
