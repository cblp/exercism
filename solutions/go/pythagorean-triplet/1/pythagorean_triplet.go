package pythagorean

import "math"

type Triplet [3]int

// Range generates list of all Pythagorean triplets with side lengths
// in the provided range.
func Range(min, max int) (r []Triplet) {
	for a := min; a < max; a++ {
		for b := a + 1; b < max; b++ {
			c2 := a*a + b*b
			if !(min*min <= c2 && c2 <= max*max) {
				continue
			}
			c := int(math.Sqrt(float64(c2)))
			if c*c == c2 {
				r = append(r, Triplet{a, b, c})
			}
		}
	}
	return
}

// Sum returns a list of all Pythagorean triplets with a certain perimeter.
func Sum(p int) (r []Triplet) {
	for a := 1; a < p/3; a++ {
		for b := a + 1; b < min(p-a, 2*p/3); b++ {
			c := p - a - b
			if c > b && a*a+b*b == c*c {
				r = append(r, Triplet{a, b, c})
			}
		}
	}
	return
}
