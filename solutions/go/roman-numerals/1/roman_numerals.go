package romannumerals

import "errors"

var ErrOutOfRange = errors.New(
	"input out of range (must be between 1 and 3999)",
)

func ToRomanNumeral(input int) (result string, err error) {
	if input <= 0 || input >= 4000 {
		return "", ErrOutOfRange
	}

	digits := []struct {
		int
		string
	}{
		{1000, "M"}, {900, "CM"}, {500, "D"}, {400, "CD"},
		{100, "C"}, {90, "XC"}, {50, "L"}, {40, "XL"},
		{10, "X"}, {9, "IX"}, {5, "V"}, {4, "IV"},
		{1, "I"},
	}

	for _, digit := range digits {
		for input >= digit.int {
			input -= digit.int
			result += digit.string
		}
	}
	return
}
