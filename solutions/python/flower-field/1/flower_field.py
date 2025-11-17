def annotate_cell(garden: list[str], i: int, j: int) -> str:
    if garden[i][j] == "*":
        return "*"
    if garden[i][j] != " ":
        raise ValueError("The board is invalid with current input.")
    mines = sum(
        garden[x][y] == "*"
        for x in [i - 1, i, i + 1]
        if 0 <= x < len(garden)
        for y in [j - 1, j, j + 1]
        if 0 <= y < len(garden[x])
    )
    return str(mines) if mines else " "


def annotate(garden: list[str]) -> list[str]:
    if not garden:
        return garden
    width = len(garden[0])
    if any(len(row) != width for row in garden):
        raise ValueError("The board is invalid with current input.")
    return [
        "".join(annotate_cell(garden, i, j) for j in range(width))
        for i, row in enumerate(garden)
    ]
