package linkedlist

import "errors"

type Node struct {
	data int
	next *Node
}

type List struct {
	head *Node
}

func New(elements []int) *List {
	var node *Node = nil
	for _, element := range elements {
		node = &Node{data: element, next: node}
	}
	return &List{head: node}
}

func (list *List) Size() (s int) {
	for node := list.head; node != nil; node = node.next {
		s++
	}
	return
}

func (list *List) Push(data int) {
	list.head = &Node{data: data, next: list.head}
}

func (list *List) Pop() (int, error) {
	if list.head == nil {
		return 0, errors.New("empty list")
	}
	data := list.head.data
	list.head = list.head.next
	return data, nil
}

func (list *List) Array() []int {
	size := list.Size()
	array := make([]int, size)
	i := size - 1
	for node := list.head; node != nil; node = node.next {
		array[i] = node.data
		i--
	}
	return array
}

func (list *List) Reverse() *List {
	out := New(nil)
	for node := list.head; node != nil; node = node.next {
		out.Push(node.data)
	}
	return out
}
