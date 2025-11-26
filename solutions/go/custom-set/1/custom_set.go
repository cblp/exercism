package stringset

import "maps"

// Implement Set as a collection of unique string values.
//
// For Set.String, use '{' and '}', output elements as double-quoted strings
// safely escaped with Go syntax, and use a comma and a single space between
// elements. For example, a set with 2 elements, "a" and "b", should be formatted as {"a", "b"}.
// Format the empty set as {}.

type Set map[string]struct{}

func New() Set {
	return Set{}
}

func NewFromSlice(l []string) Set {
	s := Set{}
	for _, x := range l {
		s.Add(x)
	}
	return s
}

func (s Set) String() string {
	r := "{"
	first := true
	for x := range s {
		if first {
			first = false
		} else {
			r += ", "
		}
		r += "\"" + x + "\""
	}
	r += "}"
	return r
}

func (s Set) IsEmpty() bool {
	return len(s) == 0
}

func (s Set) Has(elem string) bool {
	_, ok := s[elem]
	return ok
}

func (s Set) Add(elem string) {
	s[elem] = struct{}{}
}

func Subset(s1, s2 Set) bool {
	for x := range s1 {
		if !s2.Has(x) {
			return false
		}
	}
	return true
}

func Disjoint(s1, s2 Set) bool {
	for x := range s1 {
		if s2.Has(x) {
			return false
		}
	}
	return true
}

func Equal(s1, s2 Set) bool {
	return maps.Equal(s1, s2)
}

func Intersection(s1, s2 Set) Set {
	s := Set{}
	for x := range s1 {
		if s2.Has(x) {
			s.Add(x)
		}
	}
	return s
}

func Difference(s1, s2 Set) Set {
	s := Set{}
	for x := range s1 {
		if !s2.Has(x) {
			s.Add(x)
		}
	}
	return s
}

func Union(s1, s2 Set) Set {
	s := maps.Clone(s1)
	for x := range s2 {
		s.Add(x)
	}
	return s
}
