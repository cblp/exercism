package cipher

import "unicode"

type shift int

type vigenere []rune

func NewCaesar() Cipher {
	return NewShift(3)
}

func NewShift(distance int) Cipher {
	if distance <= -26 || distance == 0 || distance >= 26 {
		return nil
	}
	return shift(distance)
}

func (c shift) Encode(input string) (output string) {
	for _, r := range input {
		if unicode.IsLetter(r) {
			output += string(
				'a' + (unicode.ToLower(r)-'a'+rune(c)+26)%26,
			)
		}
	}
	return
}

func (c shift) Decode(input string) (output string) {
	for _, r := range input {
		if unicode.IsLetter(r) {
			output += string(
				'a' + (unicode.ToLower(r)-'a'-rune(c)+26)%26,
			)
		}
	}
	return
}

func NewVigenere(key string) Cipher {
	allA := true
	if key == "" {
		return nil
	}
	vkey := make([]rune, 0)
	for _, r := range key {
		if !unicode.IsLetter(r) || unicode.IsUpper(r) {
			return nil
		}
		if r != 'a' {
			allA = false
		}
		vkey = append(vkey, r-'a')
	}
	if allA {
		return nil
	}
	return vigenere(vkey)
}

func (v vigenere) Encode(input string) (output string) {
	i := 0
	for _, r := range input {
		if unicode.IsLetter(r) {
			k := rune(v[i%len(v)])
			in := unicode.ToLower(r) - 'a'
			out := (in + k) % 26
			output += string('a' + out)
			i++
		}
	}
	return
}

func (v vigenere) Decode(input string) (output string) {
	i := 0
	for _, r := range input {
		if unicode.IsLetter(r) {
			k := v[i%len(v)]
			in := unicode.ToLower(r) - 'a'
			out := (in - k + 26) % 26
			output += string('a' + out)
			i++
		}
	}
	return
}
