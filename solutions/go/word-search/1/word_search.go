package wordsearch

import (
	"errors"
	"iter"
	"slices"
	"strings"
)

type Pair [2]int

// p + q * i
func add_mul(p Pair, q Pair, i int) Pair {
	return Pair{p[0] + q[0]*i, p[1] + q[1]*i}
}

type Line struct {
	letters   string
	start     Pair // (x, y)
	direction Pair // (x, y); (-1 | 0 | +1) for each
}

func reversed(s string) string {
	r := []rune(s)
	slices.Reverse(r)
	return string(r)
}

func column(m []string, c int) string {
	b := strings.Builder{}
	for _, row := range m {
		b.WriteByte(row[c])
	}
	return b.String()
}

func diagonal(m []string, x, y, dx, dy int) string {
	b := strings.Builder{}
	height := len(m)
	width := len(m[0])
	for 0 <= x && x < width && 0 <= y && y < height {
		b.WriteByte(m[y][x])
		x += dx
		y += dy
	}
	return b.String()
}

func lines(puzzle []string) iter.Seq[Line] {
	height := len(puzzle)
	width := len(puzzle[0])
	return func(yield func(Line) bool) {
		yieldLine := func(
			letters string, startX, startY, dirX, dirY int,
		) bool {
			return yield(
				Line{
					letters:   letters,
					start:     Pair{startX, startY},
					direction: Pair{dirX, dirY},
				},
			)
		}

		for i, row := range puzzle {
			// L-R
			if !yieldLine(row, 0, i, +1, 0) {
				return
			}

			// R-L
			if !yieldLine(reversed(row), width-1, i, -1, 0) {
				return
			}
		}

		for j := range width {
			col := column(puzzle, j)

			// T-B
			if !yieldLine(col, j, 0, 0, +1) {
				return
			}

			// B-T
			if !yieldLine(reversed(col), j, height-1, 0, -1) {
				return
			}
		}

		for y := range height {
			diag := diagonal(puzzle, 0, y, +1, +1)

			// TL-BR
			if !yieldLine(diag, 0, y, +1, +1) {
				return
			}

			// BR-TL
			if !yieldLine(
				reversed(diag),
				len(diag)-1, y+len(diag)-1,
				-1, -1,
			) {
				return
			}
		}

		for y := range height {
			diag := diagonal(puzzle, 0, y, +1, -1)

			// BL-TR
			if !yieldLine(diag, 0, y, +1, -1) {
				return
			}

			// TR-BL
			if !yieldLine(
				reversed(diag),
				len(diag)-1, y-len(diag)+1,
				-1, +1,
			) {
				return
			}
		}

		for x := 1; x < width; x++ {
			y := height - 1
			diag := diagonal(puzzle, x, y, +1, -1)

			// BL-TR
			if !yieldLine(diag, x, y, +1, -1) {
				return
			}

			// TR-BL
			if !yieldLine(
				reversed(diag),
				x+len(diag)-1, y-len(diag)+1,
				-1, +1,
			) {
				return
			}
		}
	}
}

func Solve(words []string, puzzle []string) (map[string][2][2]int, error) {
	r := map[string][2][2]int{}
words:
	for _, word := range words {
		for line := range lines(puzzle) {
			offset := strings.Index(line.letters, word)
			if offset >= 0 {
				r[word] = [2][2]int{
					add_mul(
						line.start,
						line.direction,
						offset,
					),
					add_mul(
						line.start,
						line.direction,
						offset+len(word)-1,
					),
				}
				continue words
			}
		}
		return nil, errors.New("not found")
	}
	return r, nil
}
