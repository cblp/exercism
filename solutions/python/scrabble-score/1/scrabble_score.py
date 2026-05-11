LETTER_SCORES = {
    letter: score
    for score, letters in {
        1: "AEIOULNRST",
        2: "DG",
        3: "BCMP",
        4: "FHVWY",
        5: "K",
        8: "JX",
        10: "QZ",
    }.items()
    for letter in letters
}


def score(word: str) -> int:
    return sum(LETTER_SCORES.get(letter.upper(), 0) for letter in word)
