package kindergarten

import (
	"errors"
	"slices"
	"strings"
)

type Garden map[string][]string

// The diagram argument starts each row with a '\n'.  This allows Go's
// raw string literals to present diagrams in source code nicely as two
// rows flush left, for example,
//
//     diagram := `
//     VVCCGG
//     VVCCGG`

var decodePlants = map[rune]string{
	'C': "clover", 'G': "grass", 'R': "radishes", 'V': "violets",
}

func NewGarden(diagram string, children []string) (*Garden, error) {
	rows := slices.Collect(strings.Lines(diagram))[1:]
	if len(rows) != 2 ||
		len(rows[0])-1 != len(rows[1]) ||
		len(rows[1]) != 2*len(children) {
		return nil, errors.New("")
	}

	children = slices.Clone(children)
	slices.Sort(children)

	garden := Garden{}
	for i, child := range children {
		plantCodes := rows[0][i*2:i*2+2] + rows[1][i*2:i*2+2]
		plants := []string{}
		for _, plantCode := range plantCodes {
			plant, ok := decodePlants[plantCode]
			if !ok {
				return nil, errors.New("")
			}
			plants = append(plants, plant)
		}
		if _, exists := garden[child]; exists {
			return nil, errors.New("")
		}
		garden[child] = plants
	}
	return &garden, nil
}

func (g *Garden) Plants(child string) ([]string, bool) {
	plants, ok := (*g)[child]
	return plants, ok
}
