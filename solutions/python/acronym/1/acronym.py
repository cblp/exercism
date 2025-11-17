import re
from itertools import islice


def abbreviate(words: str) -> str:
    return "".join(
        "".join(islice(filter(str.isalpha, word), 0, 1)).upper()
        for word in re.split("[ -]", words)
    )
