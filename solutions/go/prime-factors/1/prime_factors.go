package prime

var primes = []int64{2, 3}

func Factors(n int64) []int64 {
	r := make([]int64, 0)
	for _, p := range primes {
		if p > n {
			break
		}
		for n%p == 0 {
			r = append(r, p)
			n /= p
		}
	}
	for x := primes[len(primes)-1] + 2; n != 1; x += 2 {
		xIsPrime := true
		for _, p := range primes {
			if x%p == 0 {
				xIsPrime = false
				break
			}
		}
		if xIsPrime {
			primes = append(primes, x)
			for n%x == 0 {
				r = append(r, x)
				n /= x
			}
		}
	}
	return r
}
