def egg_count(display_value: int) -> int:
    bits = 0
    while display_value:
        bits += display_value & 1
        display_value >>= 1
    return bits
