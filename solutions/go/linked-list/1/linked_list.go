package linkedlist

import "errors"

type Node struct {
	Value any
	next  *Node
	prev  *Node
}

type List struct {
	first *Node
	last  *Node
}

func NewList(elements ...any) *List {
	list := &List{}
	for _, element := range elements {
		list.Push(element)
	}
	return list
}

func (node *Node) Next() *Node {
	return node.next
}

func (node *Node) Prev() *Node {
	return node.prev
}

func (list *List) Unshift(v any) {
	node := &Node{Value: v}
	if list.first != nil {
		node.next = list.first
		list.first = node
		node.next.prev = node
	} else {
		list.first = node
		list.last = node
	}
}

func (list *List) Push(v any) {
	node := &Node{Value: v}
	if list.last != nil {
		node.prev = list.last
		list.last = node
		node.prev.next = node
	} else {
		list.first = node
		list.last = node
	}
}

func (list *List) Shift() (any, error) {
	if list.first == nil {
		return nil, errors.New("empty list")
	}
	node := list.first
	list.first = node.next
	if list.first != nil {
		list.first.prev = nil
	} else {
		list.last = nil
	}
	return node.Value, nil
}

func (list *List) Pop() (any, error) {
	if list.last == nil {
		return nil, errors.New("empty list")
	}
	node := list.last
	list.last = node.prev
	if list.last != nil {
		list.last.next = nil
	} else {
		list.first = nil
	}
	return node.Value, nil
}

func (list *List) Reverse() {
	for node := list.first; node != nil; node = node.prev /* 'next' after reverse becase 'prev' */ {
		tmp := node.next
		node.next = node.prev
		node.prev = tmp
	}
	tmp := list.first
	list.first = list.last
	list.last = tmp
}

func (list *List) First() *Node {
	return list.first
}

func (list *List) Last() *Node {
	return list.last
}
