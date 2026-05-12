# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


def rectangles(rows: list[str]) -> int:
    h = h_runs(rows)
    v = v_runs(rows)
    return sum(
        1
        for r0, row in enumerate(rows)
        for c0, c in enumerate(row)
        if c == "+"
        for c1 in range(c0 + 1, c0 + h[r0][c0])
        if row[c1] == "+"
        for r1 in range(r0 + 1, r0 + v[r0][c0])
        if (
            rows[r1][c0] == "+"
            and rows[r1][c1] == "+"
            and h[r1][c0] > c1 - c0
            and v[r0][c1] > r1 - r0
        )
    )


def h_runs(rows: list[str]) -> list[list[int]]:
    return [h_run(row) for row in rows]


def h_run(row: str) -> list[int]:
    h = [0] * len(row)
    run = 0
    for c, b in list(enumerate(row))[::-1]:
        run = run + 1 if b in "+-" else 0
        h[c] = run
    return h


def v_runs(rows: list[str]) -> list[list[int]]:
    ncols = max(len(row) for row in rows) if rows else 0
    v: list[list[int]] = [[0] * len(row) for row in rows]
    run = [0] * ncols
    for r, row in list(enumerate(rows))[::-1]:
        for c, b in enumerate(row):
            run[c] = run[c] + 1 if b in "+|" else 0
            v[r][c] = run[c]
    return v
