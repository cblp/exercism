package strain

func Keep[T any](s []T, fn func(T) bool) (r []T) {
	for _, v := range s {
		if fn(v) {
			r = append(r, v)
		}
	}
	return
}

func Discard[T any](s []T, fn func(T) bool) (r []T) {
	for _, v := range s {
		if !fn(v) {
			r = append(r, v)
		}
	}
	return
}
