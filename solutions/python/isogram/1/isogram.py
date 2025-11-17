def is_isogram(string: str) -> bool:
    letters = set()
    for c in string:
        if not c.isalpha():
            continue
        c = c.lower()
        if c in letters:
            return False
        letters.add(c)
    return True
