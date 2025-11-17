from collections import Counter
from typing import Iterator


def parse_words(sentence: str) -> Iterator[str]:
    current_word = ""
    apostrophe = False
    for c in sentence:
        if c.isalnum():
            if apostrophe:
                current_word += "'"
                apostrophe = False
            current_word += c.lower()
        elif c == "'" and not apostrophe and current_word:
            apostrophe = True
        else:
            apostrophe = False
            if current_word:
                yield current_word
                current_word = ""
    if current_word:
        yield current_word


def count_words(sentence: str) -> dict[str, int]:
    return Counter(parse_words(sentence))
