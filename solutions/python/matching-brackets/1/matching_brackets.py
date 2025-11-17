OPEN_PARENS = {")": "(", "]": "[", "}": "{"}


def is_paired(s: str) -> bool:
    open_paren_stack = []
    for c in s:
        if c in "([{":
            open_paren_stack.append(c)
        elif c in ")]}":
            if not open_paren_stack:
                return False
            p = open_paren_stack.pop()
            if p != OPEN_PARENS[c]:
                return False
    return not open_paren_stack
