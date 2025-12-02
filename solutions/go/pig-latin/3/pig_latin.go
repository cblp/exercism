package piglatin

import (
	"slices"
	"strings"
)

var vowels = []byte("aeiou")

func is_vowel(c byte) bool {
	return slices.Contains(vowels, c)
}

func is_vowel_or_y(c byte) bool {
	return slices.Contains(vowels, c) || c == 'y'
}

func pig(input string) string {
	// rule 1
	if is_vowel(input[0]) || input[:2] == "xr" || input[:2] == "yt" {
		return input + "ay"
	}

	// rules 2 and 4
	consonants := 1
	for !is_vowel_or_y(input[consonants]) {
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
