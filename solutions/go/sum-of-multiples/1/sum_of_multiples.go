package summultiples

type Set[A comparable] map[A]any

func setSum(s Set[int]) (r int) {
	for x := range s {
		r += x
	}
	return
}

func setUnion[A comparable](sets []Set[A]) Set[A] {
	r := Set[A]{}
	for _, s := range sets {
		for x := range s {
			r[x] = nil
		}
	}
	return r
}

func filterMap[A, B any](f func(A) *B, as []A) []B {
	r := []B{}
	for _, a := range as {
		b := f(a)
		if b != nil {
			r = append(r, *b)
		}
	}
	return r
}

func setRange(start, end, step int) Set[int] {
	if step <= 0 {
		panic("setRange: step must be positive")
	}
	s := Set[int]{}
	for i := start; i < end; i += step {
		s[i] = nil
	}
	return s
}

func SumMultiples(limit int, divisors ...int) int {
	multiples := func(divisor int) *Set[int] {
		if divisor > 0 {
			s := setRange(divisor, limit, divisor)
			return &s
		}
		return nil
	}
	return setSum(setUnion(filterMap(multiples, divisors)))
}
