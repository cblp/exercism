# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring
# pylint: disable=too-few-public-methods

from collections.abc import Callable


class ConnectGame:
    def __init__(self, board: str) -> None:
        self.board = [row.split() for row in board.strip().splitlines()]
        self.h = len(self.board)
        self.w = len(self.board[0]) if self.board else 0

    def get_winner(self) -> str:
        if self._wins("O"):
            return "O"
        if self._wins("X"):
            return "X"
        return ""

    def _at(self, i: int, j: int) -> str | None:
        if 0 <= i < self.h and 0 <= j < self.w:
            return self.board[i][j]
        return None

    def _wins(self, mark: str) -> bool:
        is_end: Callable[[int, int], bool]
        if mark == "O":
            starts = [(0, j) for j in range(self.w) if self._at(0, j) == mark]
            def is_end(i: int, _j: int) -> bool:
                return i == self.h - 1
        else:
            starts = [(i, 0) for i in range(self.h) if self._at(i, 0) == mark]
            def is_end(_i: int, j: int) -> bool:
                return j == self.w - 1

        visited: set[tuple[int, int]] = set()
        frontier: set[tuple[int, int]] = set(starts)
        while frontier:
            visited |= frontier
            if any(is_end(i, j) for i, j in frontier):
                return True
            frontier = {
                n
                for p in frontier
                for n in self._neighbors(*p)
                if self._at(*n) == mark and n not in visited
            }
        return False

    def _neighbors(self, i: int, j: int) -> list[tuple[int, int]]:
        return [
            (i - 1, j),
            (i - 1, j + 1),
            (i, j - 1),
            (i, j + 1),
            (i + 1, j - 1),
            (i + 1, j),
        ]
