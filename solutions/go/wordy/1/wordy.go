package wordy

import (
	"strconv"
	"strings"
)

func evaluateNumber(s string) (int, bool) {
	i, err := strconv.Atoi(s)
	return i, err == nil
}

func evaluate(words []string) (int, bool) {
	n := len(words)
	switch {
	case n == 1:
		return evaluateNumber(words[0])
	case words[n-2] == "plus":
		a, aOk := evaluate(words[:n-2])
		b, bOk := evaluateNumber(words[n-1])
		return a + b, aOk && bOk
	case words[n-2] == "minus":
		a, aOk := evaluate(words[:n-2])
		b, bOk := evaluateNumber(words[n-1])
		return a - b, aOk && bOk
	case n < 4:
		return 0, false
	case words[n-3] == "multiplied" && words[n-2] == "by":
		a, aOk := evaluate(words[:n-3])
		b, bOk := evaluateNumber(words[n-1])
		return a * b, aOk && bOk
	case words[n-3] == "divided" && words[n-2] == "by":
		a, aOk := evaluate(words[:n-3])
		b, bOk := evaluateNumber(words[n-1])
		return a / b, aOk && bOk
	default:
		return 0, false
	}
}

func Answer(question string) (int, bool) {
	if !strings.HasPrefix(question, "What is ") ||
		!strings.HasSuffix(question, "?") {
		return 0, false
	}
	words := strings.Fields(question[8 : len(question)-1])
	return evaluate(words)
}
