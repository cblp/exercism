from dataclasses import dataclass


@dataclass
class HighScores:
    scores: list[int]

    def latest(self) -> int:
        return self.scores[-1]

    def personal_best(self) -> int:
        return max(self.scores)

    def personal_top_three(self) -> list[int]:
        return sorted(self.scores, reverse=True)[:3]
