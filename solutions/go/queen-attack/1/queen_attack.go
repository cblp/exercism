package queenattack

import "errors"

func parse(s string) (int, int, error) {
	if !('a' <= s[0] && s[0] <= 'h') {
		return 0, 0, errors.New("bad position encoding")
	}
	if !('1' <= s[1] && s[1] <= '8') {
		return 0, 0, errors.New("bad position encoding")
	}
	return int(s[0] - 'a'), int(s[1] - '1'), nil
}

func CanQueenAttack(whitePosition, blackPosition string) (bool, error) {
	if whitePosition == blackPosition {
		return false, errors.New("wrong queens configuration")
	}
	wx, wy, err := parse(whitePosition)
	if err != nil {
		return false, err
	}
	bx, by, err := parse(blackPosition)
	if err != nil {
		return false, err
	}
	return wx == bx || wy == by || wx+wy == bx+by || wx-wy == bx-by, nil
}
