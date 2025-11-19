package blackjack

var CARDS = map[string]int{
	"two": 2, "three": 3, "four": 4, "five": 5, "six": 6, "seven": 7,
	"eight": 8, "nine": 9, "ten": 10,
	"jack": 10, "queen": 10, "king": 10,
	"ace": 11,
}

// ParseCard returns the integer value of a card following blackjack ruleset.
func ParseCard(card string) int {
	return CARDS[card]
}

// FirstTurn returns the decision for the first turn, given two cards of the
// player and one card of the dealer.
func FirstTurn(card1, card2, dealerCard string) string {
	playerValue := ParseCard(card1) + ParseCard(card2)
	dealerValue := ParseCard(dealerCard)
	if playerValue == 22 {
		return "P"
	}
	if playerValue == 21 {
		if dealerValue != 11 && dealerValue != 10 {
			return "W"
		}
		return "S"
	}
	if 17 <= playerValue && playerValue <= 20 {
		return "S"
	}
	if 12 <= playerValue && playerValue <= 16 {
		if dealerValue >= 7 {
			return "H"
		}
		return "S"
	}
	return "H"
}
