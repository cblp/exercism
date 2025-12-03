package diamond

import (
	"errors"
	"strings"
)

func row(size, step byte) string {
	letter := 'A' + step
	b := strings.Builder{}
	for range size - step {
		b.WriteRune(' ')
	}
	if step == 0 {
		b.WriteByte(letter)
	} else {
		b.WriteByte(letter)
		for range 2*step - 1 {
			b.WriteRune(' ')
		}
		b.WriteByte(letter)
	}
	for range size - step {
		b.WriteRune(' ')
	}
	return b.String()
}

func Gen(char byte) (string, error) {
	if char < 'A' || char > 'Z' {
		return "", errors.New("char must be in range [A-Z]")
	}
	size := char - 'A'
	var rows []string
	for i := range size + 1 {
		rows = append(rows, row(size, i))
	}
	for i := int(size) - 1; i >= 0; i-- {
		rows = append(rows, row(size, byte(i)))
	}
	return strings.Join(rows, "\n"), nil
}
