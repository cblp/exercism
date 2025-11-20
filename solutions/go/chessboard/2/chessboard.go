package chessboard

// if a square is occupied by a piece
type File = [8]bool

// map of eight Files, accessed with keys from "A" to "H"
type Chessboard = map[string]File

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(cb Chessboard, file string) (occupied int) {
	for _, x := range cb[file] {
		if x {
			occupied++
		}
	}
	return
}

var FILES = []string{"A", "B", "C", "D", "E", "F", "G", "H"}

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(cb Chessboard, rank int) (occupied int) {
	if rank < 1 || rank > 8 {
		return
	}
	for _, file := range FILES {
		if cb[file][rank-1] {
			occupied++
		}
	}
	return
}

// CountAll should count how many squares are present in the chessboard.
func CountAll(cb Chessboard) int {
	return 64
}

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(cb Chessboard) (occupied int) {
	for _, file := range FILES {
		occupied += CountInFile(cb, file)
	}
	return
}
