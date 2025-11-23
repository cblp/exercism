package atbash

import "unicode"

func Atbash(s string) string {
	var result []rune
	var count int

	appendResult := func(r rune) {
		if count > 0 && count%5 == 0 {
			result = append(result, ' ')
		}
		result = append(result, r)
		count++
	}

	for _, r := range s {
		switch {
		case unicode.IsLetter(r):
			appendResult('z' - (unicode.ToLower(r) - 'a'))
		case unicode.IsDigit(r):
			appendResult(r)
		}
	}

	return string(result)
}
