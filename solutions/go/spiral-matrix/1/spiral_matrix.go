package spiralmatrix

func SpiralMatrix(size int) [][]int {
	m := make([][]int, size)
	for i := range size {
		m[i] = make([]int, size)
	}

	left := 0
	right := size - 1
	top := 0
	bottom := size - 1
	x := 1
	for left <= right {
		for j := left; j <= right; j++ {
			m[top][j] = x
			x++
		}
		top++
		for i := top; i <= bottom; i++ {
			m[i][right] = x
			x++
		}
		right--
		for j := right; j >= left; j-- {
			m[bottom][j] = x
			x++
		}
		bottom--
		for i := bottom; i >= top; i-- {
			m[i][left] = x
			x++
		}
		left++
	}

	return m
}
