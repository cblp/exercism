package encode

import (
	"fmt"
	"strings"
	"unicode"
)

func RunLengthEncode(input string) (out string) {
	if input == "" {
		return ""
	}

	var currentChar rune
	count := 0

	flush := func() {
		if count != 1 {
			out += fmt.Sprint(count)
		}
		out += string(currentChar)
	}

	for _, char := range input {
		if char == currentChar {
			count++
		} else {
			if count != 0 {
				flush()
			}
			currentChar = char
			count = 1
		}
	}
	if count != 0 {
		flush()
	}
	return
}

func RunLengthDecode(input string) (out string) {
	count := 0
	for _, c := range input {
		if unicode.IsDigit(c) {
			count = count*10 + int(c-'0')
		} else {
			if count == 0 {
				out += string(c)
			} else {
				out += strings.Repeat(string(c), count)
			}
			count = 0
		}
	}
	return
}
