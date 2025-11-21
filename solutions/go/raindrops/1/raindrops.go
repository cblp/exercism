package raindrops

import "fmt"

func Convert(number int) (drops string) {
	if number%3 == 0 {
		drops += "Pling"
	}
	if number%5 == 0 {
		drops += "Plang"
	}
	if number%7 == 0 {
		drops += "Plong"
	}
	if drops == "" {
		drops = fmt.Sprint(number)
	}
	return
}
