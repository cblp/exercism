package meetup

import (
	"time"
)

type WeekSchedule string

var (
	First  WeekSchedule = "First"
	Second WeekSchedule = "Second"
	Third  WeekSchedule = "Third"
	Fourth WeekSchedule = "Fourth"
	Teenth WeekSchedule = "Teenth"
	Last   WeekSchedule = "Last"
)

func Day(
	wSched WeekSchedule, wDay time.Weekday, month time.Month, year int,
) int {
	var last time.Time
	start := map[WeekSchedule]int{
		First: 1, Second: 8, Third: 15, Fourth: 22, Teenth: 13,
		Last: 22,
	}[wSched]
	for day := start; ; day++ {
		date := time.Date(year, month, day, 0, 0, 0, 0, time.UTC)
		if date.Month() != month {
			break
		}
		if date.Weekday() != wDay {
			continue
		}
		if wSched != Last {
			return date.Day()
		}
		last = date
	}
	return last.Day()
}
