package matrix

import (
	"errors"
	"slices"
	"strconv"
	"strings"
)

type Matrix [][]int

func transform[A, B any](f func(A) B, xs []A) []B {
	ys := make([]B, 0, len(xs))
	for _, x := range xs {
		ys = append(ys, f(x))
	}
	return ys
}

func forM[A, B any](xs []A, f func(A) (B, error)) ([]B, error) {
	ys := make([]B, 0, len(xs))
	for _, x := range xs {
		y, err := f(x)
		if err != nil {
			return nil, err
		}
		ys = append(ys, y)
	}
	return ys, nil
}

func New(s string) (Matrix, error) {
	rows := strings.Split(s, "\n")
	if len(rows) < 1 {
		return nil, errors.New("too few rows")
	}
	ncols := -1
	return forM(rows, func(row string) ([]int, error) {
		cells := strings.Fields(row)
		if ncols == -1 {
			ncols = len(cells)
		}
		if len(cells) != ncols {
			return nil, errors.New("uneven rows")
		}
		return forM(cells, strconv.Atoi)
	})
}

// Cols and Rows must return the results without affecting the matrix.
func (m Matrix) Cols() [][]int {
	ncols := len(m[0])
	r := make([][]int, ncols)
	for _, row := range m {
		for j, val := range row {
			r[j] = append(r[j], val)
		}
	}
	return r
}

func (m Matrix) Rows() [][]int {
	return transform(slices.Clone, m)
}

func (m Matrix) Set(row, col, val int) bool {
	if row < 0 || row >= len(m) || col < 0 || col >= len(m[row]) {
		return false
	}
	m[row][col] = val
	return true
}
