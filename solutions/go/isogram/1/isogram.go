package isogram

import "unicode"

type unitType struct{}

var unit = unitType{}

func IsIsogram(word string) bool {
	letters := make(map[rune]unitType)
	for _, char := range word {
		if !unicode.IsLetter(char) {
			continue
		}
		char = unicode.ToLower(char)
		if _, seen := letters[char]; seen {
			return false
		}
		letters[char] = unit
	}
	return true
}
