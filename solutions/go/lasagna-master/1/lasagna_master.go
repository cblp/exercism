package lasagna

func PreparationTime(layers []string, time_per_layer int) int {
	if time_per_layer == 0 {
		time_per_layer = 2
	}
	return len(layers) * time_per_layer
}

func Quantities(layers []string) (int, float64) {
	noodles := 0
	sauce := 0.0
	for _, layer := range layers {
		switch layer {
		case "noodles":
			noodles += 50
		case "sauce":
			sauce += 0.2
		}
	}
	return noodles, sauce
}

func AddSecretIngredient(friendsList, myList []string) {
	myList[len(myList)-1] = friendsList[len(friendsList)-1]
}

func ScaleRecipe(quantities []float64, portions int) []float64 {
	new_quantities := make([]float64, len(quantities))
	for i, q := range quantities {
		new_quantities[i] = q * float64(portions) / 2.0
	}
	return new_quantities
}
