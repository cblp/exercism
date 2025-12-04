package grep

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
)

type Options struct {
	caseInsensitive   bool
	invertMatch       bool
	matchEntireLine   bool
	matchingFilesOnly bool
	prependLineNumber bool
}

func parseOptions(flags []string) Options {
	options := Options{}
	for _, flag := range flags {
		switch flag {
		case "-i":
			options.caseInsensitive = true
		case "-l":
			options.matchingFilesOnly = true
		case "-n":
			options.prependLineNumber = true
		case "-v":
			options.invertMatch = true
		case "-x":
			options.matchEntireLine = true
		default:
			panic("unknown flag " + flag)
		}
	}
	return options
}

func panic_if_some(err any) {
	if err != nil {
		panic(err)
	}
}

func Search(pattern string, flags, files []string) []string {
	options := parseOptions(flags)

	if options.matchEntireLine {
		pattern = "^" + pattern + "$"
	}
	if options.caseInsensitive {
		pattern = "(?i)" + pattern
	}
	re := regexp.MustCompile(pattern)

	r := []string{}
	for _, path := range files {
		file, err := os.Open(path)
		panic_if_some(err)
		defer file.Close()

		fileScanner := bufio.NewScanner(file)
		fileNo := 0
	file:
		for fileScanner.Scan() {
			fileNo++
			matched := re.Match(fileScanner.Bytes()) !=
				options.invertMatch
			if matched {
				if options.matchingFilesOnly {
					r = append(r, path)
					break file
				}
				rline := fileScanner.Text()
				if options.prependLineNumber {
					rline = fmt.Sprint(fileNo, ":", rline)
				}
				if len(files) > 1 {
					rline = fmt.Sprint(path, ":", rline)
				}
				r = append(r, rline)
			}
		}

		panic_if_some(fileScanner.Err())
	}
	return r
}
