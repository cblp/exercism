from typing import Optional


def aliquot_sum(n: int) -> int:
    r = 0
    x = 2
    while x * x <= n:
        if n % x == 0:
            r += x
            if x * x < n:
                r += n // x
        x += 1
    return r + 1


def classify(number: int) -> Optional[str]:
    """A perfect number equals the sum of its positive divisors.

    :param number: int a positive integer
    :return: str the classification of the input integer
    """

    if number < 1:
        raise ValueError("Classification is only possible for positive integers.")

    if number == 1:
        return "deficient"

    asum = aliquot_sum(number)
    if asum < number:
        return "deficient"
    elif asum == number:
        return "perfect"
    else:
        return "abundant"
