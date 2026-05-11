# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from dataclasses import dataclass


@dataclass
class Queen:
    row: int
    column: int

    def __init__(self, row: int, column: int):
        if row < 0:
            raise ValueError("row not positive")
        if row >= 8:
            raise ValueError("row not on board")
        if column < 0:
            raise ValueError("column not positive")
        if column >= 8:
            raise ValueError("column not on board")
        self.row = row
        self.column = column

    def can_attack(self, they: "Queen") -> bool:
        if self == they:
            raise ValueError(
                "Invalid queen position: both queens in the same square"
            )
        return (
            self.row == they.row
            or self.column == they.column
            or (self.row + self.column == they.row + they.column)
            or (self.row - self.column == they.row - they.column)
        )
