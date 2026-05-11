def slices(series: str, length: int) -> list[str]:
    if not series:
        raise ValueError("series cannot be empty")
    if length > len(series):
        raise ValueError("slice length cannot be greater than series length")
    if length == 0:
        raise ValueError("slice length cannot be zero")
    if length < 0:
        raise ValueError("slice length cannot be negative")
    return [
        series[start : start + length]
        for start in range(0, len(series) - length + 1)
    ]
