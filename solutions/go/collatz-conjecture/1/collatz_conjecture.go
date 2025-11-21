package collatzconjecture

import "errors"

func CollatzConjecture(n int) (steps int, err error) {
	if n < 1 {
		return 0, errors.New("n < 1")
	}
	for n > 1 {
		steps++
		if n%2 == 0 {
			n = n / 2
		} else {
			n = n*3 + 1
		}
	}
	return
}
