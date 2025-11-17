use std::collections::HashMap;

#[derive(Default)]
struct TeamStat {
    matches_played: u32,
    matches_won: u32,
    matches_drawn: u32,
    matches_lost: u32,
    points: u32,
}

trait GetDefault {
    fn at(&mut self, key: &str) -> &mut TeamStat;
}

impl GetDefault for HashMap<String, TeamStat> {
    fn at(&mut self, key: &str) -> &mut TeamStat {
        self.entry(key.to_string()).or_default()
    }
}

pub fn tally(match_results: &str) -> String {
    let mut r = vec![
        "Team                           | MP |  W |  D |  L |  P".to_string(),
    ];
    let mut stats: HashMap<String, TeamStat> = HashMap::new();
    for row in match_results.lines() {
        let [a, b, result] = row.split(';').collect::<Vec<_>>()[..] else {
            panic!("bad format")
        };
        for team in [a, b] {
            stats.at(team).matches_played += 1;
        }
        match result {
            "win" | "loss" => {
                let [winner, loser] =
                    if result == "win" { [a, b] } else { [b, a] };
                let winner = stats.at(winner);
                winner.matches_won += 1;
                winner.points += 3;
                stats.at(loser).matches_lost += 1;
            }
            "draw" => {
                for team in [a, b] {
                    let stat = stats.at(team);
                    stat.matches_drawn += 1;
                    stat.points += 1;
                }
            }
            _ => panic!("result = {result}"),
        }
    }
    let mut stats: Vec<(String, TeamStat)> = stats.into_iter().collect();
    stats.sort_by_key(|(name, stat)| (-(stat.points as i32), name.clone()));
    for (name, stat) in stats {
        r.push(format!(
            "{name:30} | {:2} | {:2} | {:2} | {:2} | {:2}",
            stat.matches_played,
            stat.matches_won,
            stat.matches_drawn,
            stat.matches_lost,
            stat.points
        ));
    }
    r.join("\n")
}
