package yacht

import (
	"slices"
	"sync"
)

func sum(xs []int) (s int) {
	for _, x := range xs {
		s += x
	}
	return
}

type Counter = []int

var lilStraightCounter = Counter{1, 1, 1, 1, 1, 0}
var bigStraightCounter = Counter{0, 1, 1, 1, 1, 1}

func get_face_by_count(counter Counter, target_count int) int {
	for i, count := range counter {
		if count == target_count {
			return i + 1
		}
	}
	return 0
}

func get_face_at_least(counter Counter, target_count int) int {
	for i, count := range counter {
		if count >= target_count {
			return i + 1
		}
	}
	return 0
}

func Score(dice []int, category string) int {
	counter := sync.OnceValue(func() Counter {
		c := make([]int, 6)
		for _, d := range dice {
			c[d-1]++
		}
		return c
	})

	switch category {
	case "ones":
		return counter()[0]
	case "twos":
		return 2 * counter()[1]
	case "threes":
		return 3 * counter()[2]
	case "fours":
		return 4 * counter()[3]
	case "fives":
		return 5 * counter()[4]
	case "sixes":
		return 6 * counter()[5]
	case "full house":
		doublet := get_face_by_count(counter(), 2)
		triplet := get_face_by_count(counter(), 3)
		if doublet > 0 && triplet > 0 {
			return doublet*2 + triplet*3
		}
	case "four of a kind":
		return get_face_at_least(counter(), 4) * 4
	case "little straight":
		if slices.Equal(counter(), lilStraightCounter) {
			return 30
		}
	case "big straight":
		if slices.Equal(counter(), bigStraightCounter) {
			return 30
		}
	case "choice":
		return sum(dice)
	case "yacht":
		return get_face_by_count(counter(), 5) * 10
	}
	return 0
}
