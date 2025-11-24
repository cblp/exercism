package armstrong

import "fmt"

// intPow calculates n to the mth power.
// Since the result is an int, it is assumed that m is a positive power.
func intPow(n, m int) int {
	if m == 0 {
		return 1
	}

	if m == 1 {
		return n
	}

	result := n
	for i := 2; i <= m; i++ {
		result *= n
	}
	return result
}

func IsNumber(n int) bool {
	n_str := fmt.Sprint(n)
	n_len := len(n_str)

	a := 0
	for _, digit := range n_str {
		a += intPow(int(digit-'0'), n_len)
	}

	return a == n
}
