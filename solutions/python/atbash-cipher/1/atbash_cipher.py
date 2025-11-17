def mirror_char(c: str) -> str:
    return chr(25 - ord(c) + 2 * ord("a"))


def encode(plain_text: str) -> str:
    result = ""
    count = 0
    for c in plain_text:
        if not c.isalnum():
            continue
        if count and count % 5 == 0:
            result += " "
        c = c.lower()
        result += mirror_char(c) if c.isalpha() else c
        count += 1
    return result


def decode(ciphered_text: str) -> str:
    return "".join(
        mirror_char(c) if c.isalpha() else c
        for c in ciphered_text
        if c.isalnum()
    )
