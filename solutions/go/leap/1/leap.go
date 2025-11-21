package leap

func IsLeapYear(year int) bool {
	is_year_divisible_by := func(n int) bool { return year%n == 0 }
	return is_year_divisible_by(4) &&
		!(is_year_divisible_by(100) && !is_year_divisible_by(400))
}
