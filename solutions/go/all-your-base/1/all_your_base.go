package allyourbase

import (
	"errors"
	"slices"
)

var (
	errInputBase = errors.New("input base must be >= 2")
	errDigit     = errors.New(
		"all digits must satisfy 0 <= d < input base",
	)
	errOutputBase = errors.New("output base must be >= 2")
)

func decode(base int, digits []int) (int, error) {
	if base < 2 {
		return 0, errInputBase
	}
	number := 0
	for _, digit := range digits {
		if digit < 0 || base <= digit {
			return 0, errDigit
		}
		number = number*base + digit
	}
	return number, nil
}

func encode(base int, number int) ([]int, error) {
	if base < 2 {
		return nil, errOutputBase
	}
	digits := []int{}
	for number > 0 {
		digit := number % base
		number /= base
		digits = append(digits, digit)
	}
	if len(digits) == 0 {
		return []int{0}, nil
	}
	slices.Reverse(digits)
	return digits, nil
}

func ConvertToBase(
	inputBase int, inputDigits []int, outputBase int,
) ([]int, error) {
	number, err := decode(inputBase, inputDigits)
	if err != nil {
		return nil, err
	}
	return encode(outputBase, number)
}
