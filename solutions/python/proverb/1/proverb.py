from typing import Optional


def proverb(*input_data: str, qualifier: Optional[str]) -> list[str]:
    qualifier_str = qualifier + " " if qualifier is not None else ""
    return [
        f"For want of a {a} the {b} was lost."
        for a, b in zip(input_data, input_data[1:])
    ] + [
        f"And all for the want of a {qualifier_str}{input_datum}."
        for input_datum in input_data[:1]
    ]
