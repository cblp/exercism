package piglatin

import (
	"slices"
	"strings"
)

var vowels = []rune("aeiou")

func is_vowel(c rune) bool {
	return slices.Contains(vowels, c)
}

func is_vowel_or_y(c rune) bool {
	return slices.Contains(vowels, c) || c == 'y'
}

func pig(input string) string {
	// rule 1
	prefix1 := rune(input[0])
	prefix2 := input[:2]
	if is_vowel(prefix1) || prefix2 == "xr" || prefix2 == "yt" {
		return input + "ay"
	}

	// rules 2 and 4
	consonants := 0
	if input[0] == 'y' {
		consonants++
	}
	for !is_vowel_or_y(rune(input[consonants])) {
		consonants++
	}

	if consonants > 0 {
		// rule 3
		if input[consonants-1:consonants+1] == "qu" {
			consonants++
		}

		// back to rule 2
		return input[consonants:] + input[:consonants] + "ay"
	}

	// fallback
	return input
}

func Sentence(sentence string) string {
	pig_words := []string{}
	for _, word := range strings.Fields(sentence) {
		pig_words = append(pig_words, pig(word))
	}
	return strings.Join(pig_words, " ")
}
