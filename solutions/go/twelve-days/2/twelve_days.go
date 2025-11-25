package twelve

import (
	"fmt"
	"iter"
	"slices"
	"strings"
)

var gifts = []string{
	"a Partridge in a Pear Tree", "two Turtle Doves", "three French Hens",
	"four Calling Birds", "five Gold Rings", "six Geese-a-Laying",
	"seven Swans-a-Swimming", "eight Maids-a-Milking",
	"nine Ladies Dancing", "ten Lords-a-Leaping", "eleven Pipers Piping",
	"twelve Drummers Drumming",
}

var ordinals = []string{
	"first", "second", "third", "fourth", "fifth", "sixth", "seventh",
	"eighth", "ninth", "tenth", "eleventh", "twelfth",
}

func oxfordJoin(parts []string) string {
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

func ints(first, last int) iter.Seq[int] {
	return func(yield func(int) bool) {
		for i := first; i <= last; i++ {
			if !yield(i) {
				return
			}
		}
	}
}

func transform[A, B any](f func(A) B, as iter.Seq[A]) iter.Seq[B] {
	return func(yield func(B) bool) {
		for a := range as {
			if !yield(f(a)) {
				return
			}
		}
	}
}

func Song() string {
	return strings.Join(slices.Collect(transform(Verse, ints(1, 12))), "\n")
}
