package logs

import "unicode/utf8"

var applications = map[rune]string{
	'☀': "weather",
	'❗': "recommendation",
	'🔍': "search",
}

// Application identifies the application emitting the given log.
func Application(log string) string {
	for _, char := range log {
		result, ok := applications[char]
		if ok {
			return result
		}
	}
	return "default"
}

// Replace replaces all occurrences of old with new, returning the modified log
// to the caller.
func Replace(log string, oldRune, newRune rune) (new string) {
	for _, char := range log {
		if char == oldRune {
			new += string(newRune)
		} else {
			new += string(char)
		}
	}
	return
}

// WithinLimit determines whether or not the number of characters in log is
// within the limit.
func WithinLimit(log string, limit int) bool {
	return utf8.RuneCountInString(log) <= limit
}
