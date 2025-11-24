package sublist

import "slices"

func isSublist(listSmaller, listBigger []int) bool {
	for i := 0; i <= len(listBigger)-len(listSmaller); i++ {
		if slices.Equal(listBigger[i:i+len(listSmaller)], listSmaller) {
			return true
		}
	}
	return false
}

func Sublist(l1, l2 []int) Relation {
	switch {
	case len(l1) == len(l2) && slices.Equal(l1, l2):
		return RelationEqual
	case len(l1) < len(l2) && isSublist(l1, l2):
		return RelationSublist
	case len(l1) > len(l2) && isSublist(l2, l1):
		return RelationSuperlist
	default:
		return RelationUnequal
	}
}
