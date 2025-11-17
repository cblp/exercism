def unless(a: bool, b: bool) -> bool:
    return a > b

def leap_year(year: int) -> bool:
    divisible_by = lambda n: year % n == 0
    return divisible_by(4) > (divisible_by(100) > divisible_by(400))
