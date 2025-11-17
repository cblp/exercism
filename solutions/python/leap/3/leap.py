class Bool:
    value: bool

    def __init__(self, value):
        self.value = value

    def __bool__(self):
        return self.value

    def unless(self, other):
        return self and not other

def leap_year(year: int) -> bool:
    divisible_by = lambda n: Bool(year % n == 0)
    return bool(divisible_by(4).unless(divisible_by(100).unless(divisible_by(400))))
