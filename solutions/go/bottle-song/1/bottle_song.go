package bottlesong

import (
	"fmt"
	"strings"
)

var numbers = []string{
	"no",
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine",
	"ten",
}

func suffix(i int) string {
	if i == 1 {
		return ""
	}
	return "s"
}

func firstToTitle(s string) string {
	return strings.ToUpper(s[:1]) + s[1:]
}

var line3 = "And if one green bottle should accidentally fall,"

func verse(i int) []string {
	currentTitlecased := firstToTitle(numbers[i])
	currentSuffix := suffix(i)
	nextCardinal := numbers[i-1]
	nextSuffix := suffix(i - 1)
	line12 := fmt.Sprintf(
		"%s green bottle%s hanging on the wall,",
		currentTitlecased, currentSuffix,
	)
	line4 := fmt.Sprintf(
		"There'll be %s green bottle%s hanging on the wall.",
		nextCardinal, nextSuffix,
	)
	return []string{line12, line12, line3, line4}
}

func Recite(start, take int) []string {
	r := []string{}
	for i := start; i > start-take; i-- {
		if i != start {
			r = append(r, "")
		}
		r = append(r, verse(i)...)
	}
	return r
}
