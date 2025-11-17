def find_anagrams(word: str, candidates: list[str]) -> list[str]:
    word_case_normalized = word.lower()
    word_anagram_normalized = sorted(word_case_normalized)
    return [
        candidate
        for candidate in candidates
        for candidate_case_normalized in [candidate.lower()]
        if candidate_case_normalized != word_case_normalized
        and sorted(candidate_case_normalized) == word_anagram_normalized
    ]
