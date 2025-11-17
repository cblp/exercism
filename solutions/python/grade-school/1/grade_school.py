from collections import defaultdict


class School:
    _students: dict[int, set[str]]
    _add_log: list[bool]

    def __init__(self) -> None:
        self._students = defaultdict(set)
        self._add_log = []

    def add_student(self, name: str, grade: int) -> None:
        if any(n == name for ns in self._students.values() for n in ns):
            self._add_log.append(False)
            return
        self._students[grade].add(name)
        self._add_log.append(True)

    def roster(self) -> list[str]:
        return [
            name
            for _, names in sorted(self._students.items())
            for name in sorted(names)
        ]

    def grade(self, grade: int) -> list[str]:
        return sorted(self._students[grade])

    def added(self) -> list[bool]:
        return self._add_log
