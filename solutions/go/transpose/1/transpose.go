package transpose

import "strings"

func Transpose(rows []string) (cols []string) {
	for c := 0; ; c++ {
		var col []byte
		col_has_something := false
		for _, row := range rows {
			if c < len(row) {
				col = append(col, row[c])
				col_has_something = true
			} else {
				col = append(col, 0)
			}
		}
		if !col_has_something {
			break
		}
		cols = append(
			cols,
			strings.ReplaceAll(
				strings.TrimRight(string(col), "\x00"),
				"\x00",
				" ",
			),
		)
	}
	return
}
