from collections import defaultdict
from dataclasses import dataclass


@dataclass
class TeamStat:
    matches_played: int = 0
    matches_won: int = 0
    matches_drawn: int = 0
    matches_lost: int = 0
    points: int = 0


@dataclass
class Win:
    winner: str
    loser: str


@dataclass
class Draw:
    teams: tuple[str, str]


MatchResult = Win | Draw


def parse_match_result(row: str) -> MatchResult:
    team_a, team_b, result = row.split(";")
    match result:
        case "win":
            return Win(winner=team_a, loser=team_b)
        case "loss":
            return Win(winner=team_b, loser=team_a)
        case "draw":
            return Draw((team_a, team_b))
        case _:
            raise ValueError(f"result={result}")


def tally(rows: list[str]) -> list[str]:
    stats: dict[str, TeamStat] = defaultdict(TeamStat)
    for row in rows:
        # stats[team_a].matches_played += 1
        # stats[team_b].matches_played += 1
        match parse_match_result(row):
            case Win(winner, loser):
                stats[winner].matches_played += 1
                stats[winner].matches_won += 1
                stats[winner].points += 3
                stats[loser].matches_played += 1
                stats[loser].matches_lost += 1
            case Draw(teams):
                for team in teams:
                    stats[team].matches_played += 1
                    stats[team].matches_drawn += 1
                    stats[team].points += 1
    return ["Team                           | MP |  W |  D |  L |  P"] + [
        " | ".join(
            [
                f"{team:30}",
                f"{stat.matches_played:2}",
                f"{stat.matches_won:2}",
                f"{stat.matches_drawn:2}",
                f"{stat.matches_lost:2}",
                f"{stat.points:2}",
            ]
        )
        for team, stat in sorted(
            stats.items(),
            key=lambda name_stat: (-name_stat[1].points, name_stat[0]),
        )
    ]
