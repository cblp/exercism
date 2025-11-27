package flatten

func Flatten(a any) []any {
	switch a := a.(type) {
	case nil:
		return []any{}
	case []any:
		r := []any{}
		for _, x := range a {
			r = append(r, Flatten(x)...)
		}
		return r
	default:
		return []any{a}
	}
}
