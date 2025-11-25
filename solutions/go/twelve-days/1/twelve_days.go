package twelve

import (
	"fmt"
	"slices"
	"strings"
)

var gifts = []string{
	"a Partridge in a Pear Tree",
	"two Turtle Doves",
	"three French Hens",
	"four Calling Birds",
	"five Gold Rings",
	"six Geese-a-Laying",
	"seven Swans-a-Swimming",
	"eight Maids-a-Milking",
	"nine Ladies Dancing",
	"ten Lords-a-Leaping",
	"eleven Pipers Piping",
	"twelve Drummers Drumming",
}

var ordinals = []string{"first", "second", "third",
	"fourth",
	"fifth",
	"sixth",
	"seventh",
	"eighth",
	"ninth",
	"tenth",
	"eleventh",
	"twelfth",
}

func oxfordJoin(parts []string) string {
	if len(parts) == 0 {
		return ""
	}
	if len(parts) == 1 {
		return parts[0]
	}
	return strings.Join(
		append(parts[:len(parts)-1], "and "+parts[len(parts)-1]),
		", ",
	)
}

func Verse(i int) string {
	myGifts := slices.Clone(gifts[:i])
	slices.Reverse(myGifts)
	return fmt.Sprintf(
		"On the %s day of Christmas my true love gave to me: %s.",
		ordinals[i-1], oxfordJoin(myGifts),
	)
}

func Song() string {
	verses := []string{}
	for i := 1; i <= 12; i++ {
		verses = append(verses, Verse(i))
	}
	return strings.Join(verses, "\n")
}
