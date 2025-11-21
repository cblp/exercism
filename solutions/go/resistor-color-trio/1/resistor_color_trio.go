package resistorcolortrio

import "fmt"

var colorCodes = map[string]int{
	"black":  0,
	"brown":  1,
	"red":    2,
	"orange": 3,
	"yellow": 4,
	"green":  5,
	"blue":   6,
	"violet": 7,
	"grey":   8,
	"white":  9,
}

// intPow calculates n to the mth power.
// Since the result is an int, it is assumed that m is a positive power.
func intPow(n, m int) int {
	if m == 0 {
		return 1
	}

	if m == 1 {
		return n
	}

	result := n
	for i := 2; i <= m; i++ {
		result *= n
	}
	return result
}

const (
	kilo = 1000
	mega = 1000 * kilo
	giga = 1000 * mega
)

// Label describes the resistance value given the colors of a resistor.
// The label is a string with a resistance value with an unit appended
// (e.g. "33 ohms", "470 kiloohms").
func Label(colors []string) string {
	ohms := (colorCodes[colors[0]]*10 + colorCodes[colors[1]]) *
		intPow(10, colorCodes[colors[2]])
	if ohms > giga && ohms%giga == 0 {
		return fmt.Sprint(ohms/giga, " gigaohms")
	}
	if ohms > mega && ohms%mega == 0 {
		return fmt.Sprint(ohms/mega, " megaohms")
	}
	if ohms > kilo && ohms%kilo == 0 {
		return fmt.Sprint(ohms/kilo, " kiloohms")
	}
	return fmt.Sprint(ohms, " ohms")
}
