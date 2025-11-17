from itertools import batched

FONT = [
    " _     _  _     _  _  _  _  _ ",
    "| |  | _| _||_||_ |_   ||_||_|",
    "|_|  ||_  _|  | _||_|  ||_| _|",
    "                              ",
]


DIGITS: dict[tuple[str, ...], str] = {
    tuple("".join(line) for line in digit_block): str(n)
    for n, digit_block in enumerate(batched(zip(*FONT), 3))
}


def recognize_line(input_block: tuple[str, ...]) -> str:
    inp = ["".join(col) for col in zip(*input_block)]
    if len(inp) % 3 != 0:
        raise ValueError("Number of input columns is not a multiple of three")
    return "".join(DIGITS.get(digit, "?") for digit in batched(inp, 3))


def convert(input_grid: list[str]) -> str:
    if len(input_grid) % 4 != 0:
        raise ValueError("Number of input lines is not a multiple of four")
    return ",".join(recognize_line(block) for block in batched(input_grid, 4))
