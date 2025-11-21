package luhn

import "unicode"

func Valid(id string) bool {
	checksum := 0
	digits := 0
	for i := len(id) - 1; i >= 0; i-- {
		c := rune(id[i])
		if unicode.IsSpace(c) {
			continue
		}
		if !unicode.IsDigit(c) {
			return false
		}
		digits += 1
		digit := int(c - '0')
		if digits%2 == 0 {
			digit *= 2
			if digit > 9 {
				digit -= 9
			}
		}
		checksum += digit
	}
	return digits >= 2 && checksum%10 == 0
}
