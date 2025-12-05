package minesweeper

// Annotate returns an annotated board
func Annotate(board []string) []string {
	if len(board) == 0 {
		return board
	}
	height := len(board)
	width := len(board[0])
	// if any(len(row) != width for row in board){
	//     return ValueError("The board is invalid with current input.")}

	annotate_cell := func(I, J int) byte {
		if board[I][J] != ' ' {
			return board[I][J]
		}
		var mines byte = 0
		for i := I - 1; i <= I+1; i++ {
			if !(0 <= i && i < height) {
				continue
			}
			for j := J - 1; j <= J+1; j++ {
				if !(0 <= j && j < width) ||
					(i == I && j == J) {
					continue
				}
				if board[i][j] == '*' {
					mines++
				}
			}
		}
		if mines == 0 {
			return ' '
		}
		return '0' + mines
	}

	for i := range board {
		row := []byte{}
		for j := range width {
			row = append(row, annotate_cell(i, j))
		}
		board[i] = string(row)
	}
	return board
}
