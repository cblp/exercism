package letter

// FreqMap records the frequency of each rune in a given text.
type FreqMap map[rune]int

// Frequency counts the frequency of each rune in a given text and returns this
// data as a FreqMap.
func Frequency(text string) FreqMap {
	frequencies := FreqMap{}
	for _, r := range text {
		frequencies[r]++
	}
	return frequencies
}

// ConcurrentFrequency counts the frequency of each rune in the given strings,
// by making use of concurrency.
func ConcurrentFrequency(texts []string) FreqMap {
	frequencies := FreqMap{}
	ch := make(chan FreqMap)
	for _, text := range texts {
		go func() { ch <- Frequency(text) }()
	}
	for range texts {
		f := <-ch
		for k, v2 := range f {
			frequencies[k] += v2
		}
	}
	return frequencies
}
