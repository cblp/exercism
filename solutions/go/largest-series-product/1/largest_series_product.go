package lsproduct

import (
	"errors"
	"unicode"
)

func LargestSeriesProduct(digits string, size int) (int64, error) {
	if size < 0 {
		return 0, errors.New("span must be positive or zero")
	}
	if size > len(digits) {
		return 0,
			errors.New(
				"span must be less or equal to string length",
			)
	}
	if size == 0 {
		return 1, nil
	}

	maxProduct := int64(0)
	for i := 0; i <= len(digits)-size; i++ {
		product := int64(1)
		for j := range size {
			digit := digits[i+j]
			if !unicode.IsDigit(rune(digit)) {
				return 0,
					errors.New(
						"input string must only contain digits",
					)
			}
			product *= int64(digit - '0')
		}
		if product > maxProduct {
			maxProduct = product
		}
	}
	return maxProduct, nil
}
