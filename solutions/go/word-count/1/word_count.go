package wordcount

import "unicode"

type Frequency map[string]int

func WordCount(phrase string) Frequency {
	frequency := make(Frequency)
	currentWord := ""
	apostrophe := false
	for _, c := range phrase {
		if unicode.IsLetter(c) || unicode.IsDigit(c) {
			if apostrophe {
				currentWord += "'"
				apostrophe = false
			}
			currentWord += string(unicode.ToLower(c))
		} else if c == '\'' && !apostrophe && currentWord != "" {
			apostrophe = true
		} else {
			apostrophe = false
			if currentWord != "" {
				frequency[currentWord]++
				currentWord = ""
			}
		}
	}
	if currentWord != "" {
		frequency[currentWord]++
	}
	return frequency
}
