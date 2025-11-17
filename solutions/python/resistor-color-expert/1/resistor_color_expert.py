from typing import Optional

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

TOLERANCE_PERCENT_CODES = {
    "grey": 0.05,
    "violet": 0.1,
    "blue": 0.25,
    "green": 0.5,
    "brown": 1,
    "red": 2,
    "gold": 5,
    "silver": 10,
}


def significand_and_prefix(n: int) -> tuple[float, str]:
    if n == 0:
        return (n, "")
    if n >= 10**9:
        return (n / 10**9, "giga")
    if n >= 10**6:
        return (n / 10**6, "mega")
    if n >= 10**3:
        return (n / 10**3, "kilo")
    return (n, "")


def decode_colors(colors: list[str]) -> tuple[int, Optional[float]]:
    match len(colors):
        case 1:
            [value_1_color] = colors
            ohms = COLOR_CODES[value_1_color]
            return (ohms, None)
        case 4:
            [value_1_color, value_2_color, multiplier_color, tolerance_color] = colors
            ohms = (
                COLOR_CODES[value_1_color] * 10 + COLOR_CODES[value_2_color]
            ) * 10 ** COLOR_CODES[multiplier_color]
            return (ohms, TOLERANCE_PERCENT_CODES[tolerance_color])
        case 5:
            [
                value_1_color,
                value_2_color,
                value_3_color,
                multiplier_color,
                tolerance_color,
            ] = colors
            ohms = (
                COLOR_CODES[value_1_color] * 100
                + COLOR_CODES[value_2_color] * 10
                + COLOR_CODES[value_3_color]
            ) * 10 ** COLOR_CODES[multiplier_color]
            return (ohms, TOLERANCE_PERCENT_CODES[tolerance_color])
    raise ValueError(colors)


def resistor_label(colors: list[str]) -> str:
    ohms, tolerance = decode_colors(colors)
    significand, prefix = significand_and_prefix(ohms)
    if tolerance is not None:
        return f"{significand:g} {prefix}ohms ±{tolerance}%"
    return f"{significand:g} {prefix}ohms"
