package perfect

import "errors"

type Classification string

var (
	ClassificationAbundant  Classification = "ClassificationAbundant"
	ClassificationDeficient Classification = "ClassificationDeficient"
	ClassificationPerfect   Classification = "ClassificationPerfect"

	ErrOnlyPositive = errors.New("ErrOnlyPositive")
)

func aliquot_sum(n uint) uint {
	var r uint
	var x uint = 2
	for x*x <= n {
		if n%x == 0 {
			r += x
			if x*x < n {
				r += n / x
			}
		}
		x += 1
	}
	return r + 1
}

func Classify(n int64) (Classification, error) {
	if n < 1 {
		return "", ErrOnlyPositive
	}
	if n == 1 {
		return ClassificationDeficient, nil
	}
	asum := aliquot_sum(uint(n))
	if asum < uint(n) {
		return ClassificationDeficient, nil
	}
	if asum == uint(n) {
		return ClassificationPerfect, nil
	}
	return ClassificationAbundant, nil
}
