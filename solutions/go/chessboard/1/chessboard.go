package chessboard

// if a square is occupied by a piece
type File = []bool

// map of eight Files, accessed with keys from "A" to "H"
type Chessboard = map[string]File

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(cb Chessboard, file string) int {
	r := 0
	for _, x := range cb[file] {
		if x {
			r++
		}
	}
	return r
}

var FILES = []string{"A", "B", "C", "D", "E", "F", "G", "H"}

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(cb Chessboard, rank int) int {
	if rank < 1 || rank > 8 {
		return 0
	}
	r := 0
	for _, file := range FILES {
		if cb[file][rank-1] {
			r++
		}
	}
	return r
}

// CountAll should count how many squares are present in the chessboard.
func CountAll(cb Chessboard) int {
	return 64
}

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(cb Chessboard) int {
	r := 0
	for _, file := range FILES {
		r += CountInFile(cb, file)
	}
	return r
}
