package bob

import (
	"strings"
	"unicode"
)

func filterLetters(s string) (r string) {
	for _, c := range s {
		if unicode.IsLetter(c) {
			r += string(c)
		}
	}
	return
}

func Hey(remark string) string {
	remark = strings.TrimSpace(remark)
	letters := filterLetters(remark)
	isYell := letters != "" && strings.ToUpper(letters) == letters
	isQuestion := strings.HasSuffix(remark, "?")
	isSilence := remark == ""

	if isYell && isQuestion {
		return "Calm down, I know what I'm doing!"
	}
	if isQuestion {
		return "Sure."
	}
	if isYell {
		return "Whoa, chill out!"
	}
	if isSilence {
		return "Fine. Be that way!"
	}
	return "Whatever."
}
