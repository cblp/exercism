package acronym

import (
	"strings"
	"unicode"
)

func Abbreviate(s string) (r string) {
	words := strings.FieldsFunc(
		s, func(c rune) bool { return c == ' ' || c == '-' },
	)
	for _, word := range words {
		initial := rune(word[strings.IndexFunc(word, unicode.IsLetter)])
		r += string(unicode.ToUpper(initial))
	}
	return
}
