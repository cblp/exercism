# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


class Clock:
    minutes: int

    def __init__(self, hour: int, minute: int):
        self.minutes = (hour * 60 + minute) % (24 * 60)

    def __repr__(self) -> str:
        return f"Clock({self.minutes // 60}, {self.minutes % 60})"

    def __str__(self) -> str:
        return f"{self.minutes // 60 :02}:{self.minutes % 60 :02}"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Clock) and self.minutes == other.minutes

    def __add__(self, minutes: int) -> "Clock":
        return Clock(0, self.minutes + minutes)

    def __sub__(self, minutes: int) -> "Clock":
        return Clock(0, self.minutes - minutes)
