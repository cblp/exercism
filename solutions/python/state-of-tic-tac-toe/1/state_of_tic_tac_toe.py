# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


def transpose(board: list[str]) -> list[str]:
    return ["".join(column) for column in zip(*board)]


def gamestate(board: list[str]) -> str:
    cells = "".join(board)
    x_count = cells.count("X")
    o_count = cells.count("O")
    if x_count > o_count + 1:
        raise ValueError("Wrong turn order: X went twice")
    if o_count > x_count:
        raise ValueError("Wrong turn order: O started")
    lines = (
        board
        + transpose(board)
        + [
            board[0][0] + board[1][1] + board[2][2],
            board[0][2] + board[1][1] + board[2][0],
        ]
    )
    xxx_lines = 0
    ooo_lines = 0
    for line in lines:
        if line == "XXX":
            xxx_lines += 1
        elif line == "OOO":
            ooo_lines += 1
    if xxx_lines and ooo_lines:
        raise ValueError(
            "Impossible board: game should have ended after the game was won"
        )
    if xxx_lines or ooo_lines:
        return "win"
    if x_count + o_count == 9:
        return "draw"
    return "ongoing"
