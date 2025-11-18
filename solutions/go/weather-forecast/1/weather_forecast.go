// Package weather is for weather.
package weather

var (
	// CurrentCondition is the last condition.
	CurrentCondition string

	// CurrentLocation is the last used city.
	CurrentLocation string
)

// Forecast shows the weather.
func Forecast(city, condition string) string {
	CurrentLocation, CurrentCondition = city, condition
	return CurrentLocation + " - current weather condition: " +
		CurrentCondition
}
