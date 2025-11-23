package isbn

func IsValidISBN(isbn string) bool {
	var sum int
	var count int

	for i, r := range isbn {
		if r == '-' {
			continue
		}
		if r < '0' || r > '9' {
			if r == 'X' && i == len(isbn)-1 {
				sum += 10
			} else {
				return false
			}
		} else {
			sum += int(r-'0') * (10 - count)
		}
		count++
	}

	return count == 10 && sum%11 == 0
}
