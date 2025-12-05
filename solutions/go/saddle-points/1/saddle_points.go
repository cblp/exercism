package matrix

import (
	"strconv"
	"strings"
)

type Matrix struct{ data [][]int }

type Pair [2]int

func New(s string) (*Matrix, error) {
	data := [][]int{}
	rows_str := strings.Split(s, "\n")
	for _, row_str := range rows_str {
		cells_str := strings.Fields(row_str)
		row := []int{}
		for _, cell_str := range cells_str {
			cell, err := strconv.Atoi(cell_str)
			if err != nil {
				return nil, err
			}
			row = append(row, cell)
		}
		data = append(data, row)
	}
	return &Matrix{data}, nil
}

func find_max_indexes(row []int) (r []int) {
	if len(row) == 0 {
		return
	}
	r = append(r, 0)
	for j := 1; j < len(row); j++ {
		a := row[r[0]]
		b := row[j]
		if a == b {
			r = append(r, j)
		} else if a < b {
			r = []int{j}
		}
	}
	return
}

func (m *Matrix) Saddle() (r []Pair) {
	if len(m.data) == 0 {
		return
	}

	is_min_in_column := func(i, j int) bool {
		value := m.data[i][j]
		for _, row := range m.data {
			if row[j] < value {
				return false
			}
		}
		return true
	}

	for i, row := range m.data {
		for _, j := range find_max_indexes(row) {
			if is_min_in_column(i, j) {
				r = append(r, Pair{i + 1, j + 1})
			}
		}
	}
	return
}
