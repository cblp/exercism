package pascal

func next_row(prev []int) []int {
	row := []int{1}
	for i := 1; i < len(prev); i++ {
		row = append(row, prev[i-1]+prev[i])
	}
	row = append(row, 1)
	return row
}

func Triangle(n int) [][]int {
	rows := [][]int{}
	row := []int{1}
	for range n {
		rows = append(rows, row)
		row = next_row(row)
	}
	return rows
}
