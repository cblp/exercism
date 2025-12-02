package allergies

var allergies = map[string]int{
	"eggs":         0,
	"peanuts":      1,
	"shellfish":    2,
	"strawberries": 3,
	"tomatoes":     4,
	"chocolate":    5,
	"pollen":       6,
	"cats":         7,
}

func Allergies(allergyBitSet uint) []string {
	r := []string{}
	for name, id := range allergies {
		if allergyBitSet&(1<<id) != 0 {
			r = append(r, name)
		}
	}
	return r
}

func AllergicTo(allergyBitSet uint, allergen string) bool {
	return allergyBitSet&(1<<allergies[allergen]) != 0
}
