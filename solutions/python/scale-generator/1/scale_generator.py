# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


from dataclasses import dataclass
from collections.abc import Iterator

_SHARPS = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]
_FLATS = ["A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab"]
_SHARP_KEYS = {
    "C",
    "a",
    "G",
    "D",
    "A",
    "E",
    "B",
    "F#",
    "e",
    "b",
    "f#",
    "c#",
    "g#",
    "d#",
}
_FLAT_KEYS = {
    "F",
    "Bb",
    "Eb",
    "Ab",
    "Db",
    "Gb",
    "d",
    "g",
    "c",
    "f",
    "bb",
    "eb",
}


@dataclass
class Scale:
    tonic: str

    def chromatic(self) -> list[str]:
        if self.tonic in _SHARP_KEYS:
            scale = _SHARPS
        elif self.tonic in _FLAT_KEYS:
            scale = _FLATS
        else:
            raise ValueError("Bad tonic", self.tonic)
        i = scale.index(self.tonic[0].upper() + self.tonic[1:])
        return scale[i:] + scale[:i]

    def interval(self, intervals: str) -> list[str]:
        return list(self._interval(intervals))

    def _interval(self, intervals: str) -> Iterator[str]:
        ch = self.chromatic()
        n = 0
        yield ch[n]
        for i in intervals:
            n += {"m": 1, "M": 2, "A": 3}[i]
            yield ch[n % len(ch)]
