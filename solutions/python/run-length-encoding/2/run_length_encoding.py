def decode(string: str) -> str:
    result = ""
    number = 0
    for char in string:
        if char.isdigit():
            number = number * 10 + int(char)
        else:
            result += char * max(1, number)
            number = 0
    return result


def encode(string: str) -> str:
    result = ""
    number = 0
    known_char = ""

    def append_result() -> None:
        nonlocal result
        if not known_char:
            return
        if number != 1:
            result += str(number)
        result += known_char

    for char in string:
        if char == known_char:
            number += 1
        else:
            append_result()
            number = 1
            known_char = char
    append_result()
    return result
