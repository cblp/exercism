package series

func All(n int, s string) []string {
	r := []string{}
	for i := 0; i <= len(s)-n; i++ {
		r = append(r, s[i:i+n])
	}
	return r
}

func UnsafeFirst(n int, s string) string {
	return s[:n]
}

func First(n int, s string) (first string, ok bool) {
	if n > len(s) {
		return "", false
	}
	return s[:n], true
}
