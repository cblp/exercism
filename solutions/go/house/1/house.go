package house

import "strings"

var verses = []string{
	"This is the house that Jack built.",
	"This is the malt\n" +
		"that lay in the house that Jack built.",
	"This is the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the dog\n" +
		"that worried the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the cow with the crumpled horn\n" +
		"that tossed the dog\n" +
		"that worried the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the maiden all forlorn\n" +
		"that milked the cow with the crumpled horn\n" +
		"that tossed the dog\n" +
		"that worried the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the man all tattered and torn\n" +
		"that kissed the maiden all forlorn\n" +
		"that milked the cow with the crumpled horn\n" +
		"that tossed the dog\n" +
		"that worried the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the priest all shaven and shorn\n" +
		"that married the man all tattered and torn\n" +
		"that kissed the maiden all forlorn\n" +
		"that milked the cow with the crumpled horn\n" +
		"that tossed the dog\n" +
		"that worried the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the rooster that crowed in the morn\n" +
		"that woke the priest all shaven and shorn\n" +
		"that married the man all tattered and torn\n" +
		"that kissed the maiden all forlorn\n" +
		"that milked the cow with the crumpled horn\n" +
		"that tossed the dog\n" +
		"that worried the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the farmer sowing his corn\n" +
		"that kept the rooster that crowed in the morn\n" +
		"that woke the priest all shaven and shorn\n" +
		"that married the man all tattered and torn\n" +
		"that kissed the maiden all forlorn\n" +
		"that milked the cow with the crumpled horn\n" +
		"that tossed the dog\n" +
		"that worried the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
	"This is the horse and the hound and the horn\n" +
		"that belonged to the farmer sowing his corn\n" +
		"that kept the rooster that crowed in the morn\n" +
		"that woke the priest all shaven and shorn\n" +
		"that married the man all tattered and torn\n" +
		"that kissed the maiden all forlorn\n" +
		"that milked the cow with the crumpled horn\n" +
		"that tossed the dog\n" +
		"that worried the cat\n" +
		"that killed the rat\n" +
		"that ate the malt\n" +
		"that lay in the house that Jack built.",
}

func Verse(v int) string {
	return verses[v-1]
}

func Song() string {
	return strings.Join(verses, "\n\n")
}
