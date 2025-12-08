package rectangles

func Count(diagram []string) (count int) {
	is_vertical := func(b byte) bool {
		return b == '|' || b == '+'
	}

	is_horizontal := func(b byte) bool {
		return b == '-' || b == '+'
	}

	check := func(left, top, right, bottom int) bool {
		if diagram[top][left] != '+' ||
			diagram[top][right] != '+' ||
			diagram[bottom][left] != '+' ||
			diagram[bottom][right] != '+' {
			return false
		}
		for i := top + 1; i < bottom; i++ {
			if !is_vertical(diagram[i][left]) ||
				!is_vertical(diagram[i][right]) {
				return false
			}
		}
		for j := left + 1; j < right; j++ {
			if !is_horizontal(diagram[top][j]) ||
				!is_horizontal(diagram[bottom][j]) {
				return false
			}
		}
		return true
	}

	for top := range len(diagram) {
		width := len(diagram[0])
		for left := range width {
			for bottom := top + 1; bottom < len(diagram); bottom++ {
				for right := left + 1; right < width; right++ {
					if check(left, top, right, bottom) {
						count++
					}
				}
			}
		}
	}
	return
}
