"""Solution to Ellen's Alien Game exercise."""

from typing import ClassVar, Optional


class Alien:
    """Create an Alien object with location x_coordinate and y_coordinate.

    Methods
    -------
    collision_detection(other): Implementation TBD.
    """

    total_aliens_created: ClassVar[int] = 0

    x_coordinate: int
    y_coordinate: int
    health: int

    def __init__(self, x_coordinate: int, y_coordinate: int):
        self.x_coordinate = x_coordinate
        self.y_coordinate = y_coordinate
        self.health = 3
        Alien.total_aliens_created += 1

    def hit(self) -> None:
        """Decrement Alien health by one point"""
        self.health -= 1

    def is_alive(self) -> bool:
        """Alien is alive (if health is > 0)"""
        return self.health > 0

    def teleport(self, new_x_coordinate: int, new_y_coordinate: int) -> None:
        """Move Alien object to new coordinates"""
        self.x_coordinate = new_x_coordinate
        self.y_coordinate = new_y_coordinate

    def collision_detection(self, other: "Alien") -> Optional[object]:
        pass


def new_aliens_collection(positions: list[tuple[int, int]]) -> list[Alien]:
    return [Alien(x, y) for x, y in positions]
