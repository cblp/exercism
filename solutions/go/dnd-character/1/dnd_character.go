package dndcharacter

import (
	"math/rand"
	"sort"
)

type Character struct {
	Strength     int
	Dexterity    int
	Constitution int
	Intelligence int
	Wisdom       int
	Charisma     int
	Hitpoints    int
}

// Modifier calculates the ability modifier for a given ability score
func Modifier(score int) int {
	return score/2 - 5
}

// Ability uses randomness to generate the score for an ability
func Ability() int {
	d6 := func() int {
		return rand.Intn(6) + 1
	}
	fourD6 := []int{d6(), d6(), d6(), d6()}
	sort.Ints(fourD6)
	return fourD6[1] + fourD6[2] + fourD6[3]
}

// GenerateCharacter creates a new Character with random scores for abilities
func GenerateCharacter() Character {
	Constitution := Ability()
	return Character{
		Strength:     Ability(),
		Dexterity:    Ability(),
		Constitution: Constitution,
		Intelligence: Ability(),
		Wisdom:       Ability(),
		Charisma:     Ability(),
		Hitpoints:    10 + Modifier(Constitution),
	}
}
