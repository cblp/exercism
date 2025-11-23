package prime

import "errors"

var primes = []int{2, 3}

// Nth returns the nth prime number.
// An error must be returned if the nth prime number can't be calculated
// ('n' is equal or less than zero)
func Nth(n int) (int, error) {
	if n < 1 {
		return 0, errors.New("there is no zeroth prime")
	}
	for n > len(primes) {
		for x := primes[len(primes)-1] + 2; ; x += 2 {
			isPrime := true
			for _, p := range primes {
				if p*p > x {
					break
				}
				if x%p == 0 {
					isPrime = false
					break
				}
			}
			if isPrime {
				primes = append(primes, x)
				break
			}
		}
	}
	return primes[n-1], nil
}
