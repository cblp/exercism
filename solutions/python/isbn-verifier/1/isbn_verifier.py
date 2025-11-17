def is_valid(isbn: str) -> bool:
    checksum = 0
    factor = 10
    for c in isbn:
        if factor == 0:
            return False
        if c == "-":
            continue
        if c.isdecimal():
            checksum += int(c) * factor
        elif factor == 1 and c == "X":
            checksum += 10
        else:
            return False
        factor -= 1
    return factor == 0 and checksum % 11 == 0
