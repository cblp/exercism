package etl

import (
	"strings"
)

func Transform(in map[int][]string) map[string]int {
	r := make(map[string]int)
	for score, letters := range in {
		for _, letter := range letters {
			letter = strings.ToLower(letter)
			r[letter] = score
		}
	}
	return r
}
