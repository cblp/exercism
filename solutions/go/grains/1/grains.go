package grains

import "errors"

func Square(number int) (uint64, error) {
	if number < 1 || number > 64 {
		return 0, errors.New("square number must be between 1 and 64")
	}
	return 1 << (number - 1), nil
}

func Total() uint64 {
	return ^uint64(0)
}
