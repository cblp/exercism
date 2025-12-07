package brackets

var OPEN_PARENS = map[rune]rune{')': '(', ']': '[', '}': '{'}

func Bracket(input string) bool {
	parens := []rune{}
	for _, c := range input {
		switch c {
		case '(', '[', '{':
			parens = append(parens, c)
		case ')', ']', '}':
			if len(parens) == 0 {
				return false
			}
			p := parens[len(parens)-1]
			parens = parens[:len(parens)-1]
			if p != OPEN_PARENS[c] {
				return false
			}
		}
	}
	return len(parens) == 0
}
