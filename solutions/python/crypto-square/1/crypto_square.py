# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from itertools import batched, count


def cipher_text(plain_text: str) -> str:
    message = [c.lower() for c in plain_text if c.isalpha() or c.isdigit()]
    if not message:
        return ""
    cols = next(c for r in count() for c in [r, r + 1] if c * r >= len(message))
    plain_square = ["".join(line) for line in batched(message, cols)]
    last_line = plain_square[-1]
    plain_square_padded = plain_square[:-1] + [
        last_line + (cols - len(last_line)) * " "
    ]
    return " ".join("".join(col) for col in zip(*plain_square_padded))
