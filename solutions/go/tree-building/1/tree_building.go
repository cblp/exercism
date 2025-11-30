package tree

import (
	"errors"
	"fmt"
	"maps"
	"slices"
)

type unitType = struct{}

var unit = unitType{}

type set map[int]unitType

type Record struct {
	ID     int
	Parent int
}

type Node struct {
	ID       int
	Children []*Node
}

func childrenFromMap(mapFromParentToChildren map[int]set, parent int) []*Node {
	childrenSet := mapFromParentToChildren[parent]
	if childrenSet == nil {
		return nil
	}

	childrenIds := slices.Collect(maps.Keys(childrenSet))
	slices.Sort(childrenIds)

	children := []*Node{}
	for _, child := range childrenIds {
		if parent == child {
			panic(fmt.Sprintf("parent=%d, child=%d", parent, child))
		}
		grandchildren := childrenFromMap(mapFromParentToChildren, child)
		childNode := Node{ID: child, Children: grandchildren}
		children = append(children, &childNode)
	}
	return children
}

func first(s set) int {
	for x := range s {
		return x
	}
	panic("")
}

func minMax(s set) (int, int) {
	lo := first(s)
	hi := lo
	for x := range s {
		if x < lo {
			lo = x
		}
		if x > hi {
			hi = x
		}
	}
	return lo, hi
}

func Build(records []Record) (*Node, error) {
	mapFromParentToChildren := map[int]set{}
	idSet := set{}
	rootExists := false
	for _, r := range records {
		idSet[r.ID] = unit

		if r.Parent == 0 && r.ID == 0 {
			if rootExists {
				return nil, errors.New("duplicate root")
			}
			rootExists = true
			continue
		}
		if !(r.Parent < r.ID) {
			return nil, fmt.Errorf("causality fail: %#v", r)
		}
		if _, exists := mapFromParentToChildren[r.Parent][r.ID]; exists {
			return nil, errors.New("duplicate node")
		}

		if mapFromParentToChildren[r.Parent] == nil {
			mapFromParentToChildren[r.Parent] = set{}
		}
		mapFromParentToChildren[r.Parent][r.ID] = unit
	}

	if !rootExists {
		if len(mapFromParentToChildren) != 0 {
			return nil, errors.New("no root")
		}
		return nil, nil
	}

	lo, hi := minMax(idSet)
	if lo != 0 || hi != len(idSet)-1 {
		return nil, errors.New("non-continuous ids")
	}

	children := childrenFromMap(mapFromParentToChildren, 0)
	return &Node{ID: 0, Children: children}, nil
}
