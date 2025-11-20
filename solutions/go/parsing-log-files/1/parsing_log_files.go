package parsinglogfiles

import "regexp"

func IsValidLine(text string) bool {
	return regexp.
		MustCompile(`^\[(DBG|ERR|FTL|INF|TRC|WRN)\]`).
		MatchString(text)
}

func SplitLogLine(text string) []string {
	return regexp.MustCompile(`<[~*=-]*>`).Split(text, 3)
}

func CountQuotedPasswords(lines []string) (count int) {
	passwordRe := regexp.MustCompile(`(?i)".*password.*"`)
	for _, line := range lines {
		if passwordRe.MatchString(line) {
			count++
		}
	}
	return
}

func RemoveEndOfLineText(text string) string {
	return regexp.
		MustCompile(`end-of-line\d+`).
		ReplaceAllLiteralString(text, "")
}

func TagWithUserName(lines []string) []string {
	userRe := regexp.MustCompile(`User +([A-Za-z0-9]+)`)
	for i, line := range lines {
		match := userRe.FindStringSubmatch(line)
		if match != nil {
			lines[i] = "[USR] " + match[1] + " " + line
		}
	}
	return lines
}
