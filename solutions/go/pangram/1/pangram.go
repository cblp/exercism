package pangram

import "unicode"

type unitType struct{}

var unit = unitType{}

type Set map[rune]unitType

func toSet(s string) Set {
	set := make(Set)
	for _, r := range s {
		set[unicode.ToLower(r)] = unit
	}
	return set
}

func isSubsetOf(seq string, set Set) bool {
	for _, r := range seq {
		if _, found := set[r]; !found {
			return false
		}
	}
	return true
}

var alphabet = "abcdefghijklmnopqrstuvwxyz"

func IsPangram(input string) bool {
	return isSubsetOf(alphabet, toSet(input))
}
