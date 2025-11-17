COLOR_CODES = {
    "black": 0,
    "brown": 1,
    "red": 2,
    "orange": 3,
    "yellow": 4,
    "green": 5,
    "blue": 6,
    "violet": 7,
    "grey": 8,
    "white": 9,
}


def label(colors: list[str]) -> str:
    a, b, c, *_ = [COLOR_CODES[color] for color in colors]
    ohms = (a * 10 + b) * 10**c
    if ohms > 10**9 and ohms % 10**9 == 0:
        return f"{ohms // 10 ** 9} gigaohms"
    if ohms > 10**6 and ohms % 10**6 == 0:
        return f"{ohms // 10 ** 6} megaohms"
    if ohms > 10**3 and ohms % 10**3 == 0:
        return f"{ohms // 10 ** 3} kiloohms"
    return f"{ohms} ohms"
