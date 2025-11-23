package clock

import "fmt"

type Clock struct {
	minutes int
}

const DAY_MINUTES = 24 * 60

func divMod(a int, b uint) (int, int) {
	m := int(b)
	if a < 0 {
		return (a+1)/m - 1, (a%m + m) % m
	}
	return a / m, a % m
}

func New(h, m int) Clock {
	_, minutes := divMod(h*60+m, DAY_MINUTES)
	return Clock{minutes}
}

func (c Clock) Add(m int) Clock {
	_, minutes := divMod(c.minutes+m, DAY_MINUTES)
	return Clock{minutes}
}

func (c Clock) Subtract(m int) Clock {
	_, minutes := divMod(c.minutes-m, DAY_MINUTES)
	return Clock{minutes}
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.minutes/60, c.minutes%60)
}
