package cryptosquare

import (
	"slices"
	"strings"
	"unicode"
)

func transform(f func([]rune) string, as [][]rune) []string {
	bs := []string{}
	for _, a := range as {
		bs = append(bs, f(a))
	}
	return bs
}

func filterMap(f func(rune) rune, as string) []rune {
	bs := []rune{}
	for _, a := range as {
		b := f(a)
		if b != 0 {
			bs = append(bs, b)
		}
	}
	return bs
}

func transpose(rows [][]rune) [][]rune {
	width := len(rows[0])
	r := [][]rune{}
	for i := range width {
		s := []rune{}
		for _, row := range rows {
			s = append(s, row[i])
		}
		r = append(r, s)
	}
	return r
}

func runesToString(rs []rune) string { return string(rs) }

func Encode(plainText string) string {
	message := filterMap(func(r rune) rune {
		if unicode.IsLetter(r) || unicode.IsDigit(r) {
			return unicode.ToLower(r)
		}
		return 0
	}, plainText)

	c := 0
	r := 0
	for c*r < len(message) {
		if r < c {
			r++
		} else {
			c++
		}
	}
	if c == 0 {
		return ""
	}

	plainSquare := slices.Collect(slices.Chunk(message, c))
	lastLine := plainSquare[len(plainSquare)-1]
	if len(lastLine) < c {
		plainSquare[len(plainSquare)-1] = append(
			lastLine,
			slices.Repeat([]rune{' '}, c-len(lastLine))...,
		)
	}

	cipherSquare := transpose(plainSquare)
	cipherText := strings.Join(transform(runesToString, cipherSquare), " ")
	return cipherText
}
