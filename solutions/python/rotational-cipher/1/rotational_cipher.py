def rotate_letter(char: str, base_code: int, key: int) -> str:
    return chr((ord(char) - base_code + key) % 26 + base_code)


def rotate_char(char: str, key: int) -> str:
    if char.isupper():
        return rotate_letter(char, ord("A"), key)
    if char.islower():
        return rotate_letter(char, ord("a"), key)
    return char


def rotate(text: str, key: int) -> str:
    return "".join(rotate_char(char, key) for char in text)
