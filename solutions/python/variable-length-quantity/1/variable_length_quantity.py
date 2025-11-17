def to_base_128(n: int) -> list[int]:
    digits = []
    while n:
        digit = n % 128
        n = n // 128
        digits.append(digit)
    digits.reverse()
    return digits or [0]


def encode(numbers: list[int]) -> list[int]:
    result = []
    for number in numbers:
        digits = to_base_128(number)
        for i, digit in enumerate(digits):
            result.append((0x80 if i != len(digits) - 1 else 0) | digit)
    return result


def decode(input_bytes: list[int]) -> list[int]:
    result = []
    number = 0
    in_sequence = False
    for byte in input_bytes:
        number = number * 128 + (byte & 0x7F)
        in_sequence = bool(byte & 0x80)
        if not in_sequence:
            result.append(number)
            number = 0
    if in_sequence:
        raise ValueError("incomplete sequence")
    return result
