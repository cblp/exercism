package rotationalcipher

import "unicode"

func RotationalCipher(plain string, shiftKey int) (r string) {
	for _, c := range plain {
		switch {
		case unicode.IsLower(c):
			c = ((c - 'a' + rune(shiftKey)) % 26) + 'a'
		case unicode.IsUpper(c):
			c = ((c - 'A' + rune(shiftKey)) % 26) + 'A'
		}
		r += string(c)
	}
	return
}
