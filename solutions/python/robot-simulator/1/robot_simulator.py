from dataclasses import dataclass, astuple


@dataclass
class Vector:
    x: int
    y: int

    def __add__(self, other: "Vector") -> "Vector":
        return Vector(self.x + other.x, self.y + other.y)

    def dot(self, other: tuple[int, int]) -> int:
        return self.x * other[0] + self.y * other[1]

    def __mul__(self, m: "Matrix") -> "Vector":
        return Vector(self.dot(m.row1), self.dot(m.row2))


@dataclass
class Matrix:
    row1: tuple[int, int]
    row2: tuple[int, int]


Direction = Vector

EAST = Direction(1, 0)
NORTH = Direction(0, 1)
WEST = Direction(-1, 0)
SOUTH = Direction(0, -1)

ROTATE_LEFT = Matrix((0, -1), (1, 0))
ROTATE_RIGHT = Matrix((0, 1), (-1, 0))


class Robot:
    _coordinates: Vector
    direction: Direction

    def __init__(
        self, direction: Direction = NORTH, x_pos: int = 0, y_pos: int = 0
    ):
        self._coordinates = Vector(x_pos, y_pos)
        self.direction = direction

    @property
    def coordinates(self) -> tuple[int, int]:
        return astuple(self._coordinates)

    def move(self, commands: str) -> None:
        for command in commands:
            {"A": self.advance, "L": self.turn_left, "R": self.turn_right}[
                command
            ]()

    def advance(self) -> None:
        self._coordinates += self.direction

    def turn_left(self) -> None:
        self.direction *= ROTATE_LEFT

    def turn_right(self) -> None:
        self.direction *= ROTATE_RIGHT
