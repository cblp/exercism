def transform(legacy_data: dict[int, list[str]]) -> dict[str, int]:
    return {
        letter.lower(): count
        for count, letters in legacy_data.items()
        for letter in letters
    }
