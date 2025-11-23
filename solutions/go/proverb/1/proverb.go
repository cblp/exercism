package proverb

func Proverb(rhyme []string) []string {
	if len(rhyme) == 0 {
		return []string{}
	}

	var result []string
	for i := 0; i < len(rhyme)-1; i++ {
		line := "For want of a " + rhyme[i] + " the " + rhyme[i+1] +
			" was lost."
		result = append(result, line)
	}
	finalLine := "And all for the want of a " + rhyme[0] + "."
	result = append(result, finalLine)

	return result
}
