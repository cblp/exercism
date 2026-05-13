# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


def encode(message: str, rails: int) -> str:
    return subst(message, rails, False)


def decode(message: str, rails: int) -> str:
    return subst(message, rails, True)


def subst(message: str, rails: int, is_decoding: bool) -> str:
    max_step = 2 * (rails - 1)
    out = [""] * len(message)
    cipher_ix = 0
    for r in range(0, rails):
        step = 2 * r
        plain_ix = r
        while plain_ix < len(message):
            if is_decoding:
                out[plain_ix] = message[cipher_ix]
            else:
                out[cipher_ix] = message[plain_ix]
            cipher_ix += 1
            if step != max_step:
                step = max_step - step
            plain_ix += step
    return "".join(out)
