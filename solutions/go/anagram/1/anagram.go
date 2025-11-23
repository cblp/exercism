package anagram

import (
	"slices"
	"strings"
)

func Detect(subject string, candidates []string) (anagrams []string) {
	sorted := func(s string) []rune {
		runes := []rune(s)
		slices.Sort(runes)
		return runes
	}

	subjectLower := strings.ToLower(subject)
	subjectSorted := sorted(subjectLower)

	isAnagram := func(condidate string) bool {
		condidateLower := strings.ToLower(condidate)
		return condidateLower != subjectLower &&
			slices.Equal(sorted(condidateLower), subjectSorted)
	}

	for _, candidate := range candidates {
		if isAnagram(candidate) {
			anagrams = append(anagrams, candidate)
		}
	}
	return
}
