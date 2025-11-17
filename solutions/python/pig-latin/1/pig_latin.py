def is_ascii_vowel(a: str) -> bool:
    return a in "aeiou"


def is_vowel_or_y(a: str) -> bool:
    return a in "aeiouy"


def starts_with_vowel(input: str) -> bool:
    return bool(input) and is_ascii_vowel(input[0])


def translate_word(input: str) -> str:
    # rule 1
    if starts_with_vowel(input) or input.startswith("xr") or input.startswith("yt"):
        return input + "ay"

    # rules 2 and 4
    consonants_count = 0
    if input.startswith("y"):
        consonants_count += 1
    while not is_vowel_or_y(input[consonants_count]):
        consonants_count += 1

    if consonants_count > 0:
        # rule 3
        if input[consonants_count - 1 : consonants_count + 1] == "qu":
            consonants_count += 1

        # back to rule 2
        return input[consonants_count:] + input[:consonants_count] + "ay"

    # fallback
    return input


def translate(text: str) -> str:
    return " ".join(translate_word(word) for word in text.split())
