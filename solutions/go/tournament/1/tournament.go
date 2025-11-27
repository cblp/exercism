package tournament

import (
	"bufio"
	"cmp"
	"fmt"
	"io"
	"maps"
	"slices"
	"strings"
)

type teamStat struct {
	name           string
	matches_played uint
	matches_won    uint
	matches_drawn  uint
	matches_lost   uint
	points         uint
}

type teamStats map[string]*teamStat

func (stats teamStats) at(team string) *teamStat {
	stat := stats[team]
	if stat != nil {
		return stat
	}
	stat = &teamStat{name: team}
	stats[team] = stat
	return stat
}

func Tally(reader io.Reader, writer io.Writer) error {
	fmt.Fprintf(writer, "%-30s | MP |  W |  D |  L |  P\n", "Team")
	scanner := bufio.NewScanner(reader)
	statsCollecting := teamStats{}
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		fields := strings.Split(line, ";")
		if len(fields) != 3 {
			return fmt.Errorf(
				"expected 3 fields in a row, got %d",
				len(fields),
			)
		}
		for _, team := range fields[:2] {
			statsCollecting.at(team).matches_played += 1
		}
		recordWin := func(winner, loser string) {
			winnerStat := statsCollecting.at(winner)
			winnerStat.matches_won += 1
			winnerStat.points += 3
			statsCollecting.at(loser).matches_lost += 1
		}
		switch fields[2] {
		case "win":
			recordWin(fields[0], fields[1])
		case "loss":
			recordWin(fields[1], fields[0])
		case "draw":
			for _, team := range fields[:2] {
				stat := statsCollecting.at(team)
				stat.matches_drawn += 1
				stat.points += 1
			}
		default:
			return fmt.Errorf(
				"expected win|loss|draw, got %#v", fields[2],
			)
		}
	}
	if err := scanner.Err(); err != nil {
		return err
	}
	stats := slices.SortedFunc(
		maps.Values(statsCollecting),
		func(a, b *teamStat) int {
			return cmp.Or(
				cmp.Compare(b.points, a.points),
				strings.Compare(a.name, b.name),
			)
		},
	)
	for _, stat := range stats {
		fmt.Fprintf(
			writer,
			"%-30s | %2d | %2d | %2d | %2d | %2d\n",
			stat.name,
			stat.matches_played,
			stat.matches_won,
			stat.matches_drawn,
			stat.matches_lost,
			stat.points,
		)
	}
	return nil
}
