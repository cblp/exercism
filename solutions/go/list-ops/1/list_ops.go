package listops

// IntList is an abstraction of a list of integers which we can define methods on
type IntList []int

func (s IntList) Foldl(fn func(int, int) int, initial int) int {
	r := initial
	for _, x := range s {
		r = fn(r, x)
	}
	return r
}

func (s IntList) Foldr(fn func(int, int) int, initial int) int {
	r := initial
	for i := len(s) - 1; i >= 0; i-- {
		r = fn(s[i], r)
	}
	return r
}

func (s IntList) Filter(fn func(int) bool) IntList {
	r := IntList{}
	for _, x := range s {
		if fn(x) {
			r = append(r, x)
		}
	}
	return r
}

func (s IntList) Length() int {
	return len(s)
}

func (s IntList) Map(fn func(int) int) IntList {
	r := IntList{}
	for _, x := range s {
		r = append(r, fn(x))
	}
	return r
}

func (s IntList) Reverse() IntList {
	r := IntList{}
	for i := len(s) - 1; i >= 0; i-- {
		r = append(r, s[i])
	}
	return r
}

func (s IntList) Append(lst IntList) IntList {
	return append(s, lst...)
}

func (s IntList) Concat(lists []IntList) IntList {
	r := s
	for _, list := range lists {
		r = append(r, list...)
	}
	return r
}
