from typing import Optional

FLOWERS = {"V": "Violets", "R": "Radishes", "C": "Clover", "G": "Grass"}

DEFAULT_STUDENTS = [
    "Alice",
    "Bob",
    "Charlie",
    "David",
    "Eve",
    "Fred",
    "Ginny",
    "Harriet",
    "Ileana",
    "Joseph",
    "Kincaid",
    "Larry",
]


class Garden:
    _plants: dict[str, list[str]]

    def __init__(self, diagram: str, students: Optional[list[str]] = None):
        students = (
            sorted(students) if students is not None else DEFAULT_STUDENTS
        )
        row1, row2 = diagram.splitlines()
        self._plants = {}
        for i, student in enumerate(students):
            self._plants[student] = [
                FLOWERS[flower]
                for flower in row1[2 * i : 2 * (i + 1)]
                + row2[2 * i : 2 * (i + 1)]
            ]

    def plants(self, student: str) -> list[str]:
        return self._plants[student]
