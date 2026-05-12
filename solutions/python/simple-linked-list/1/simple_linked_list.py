# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from copy import copy
from typing import Iterable, Iterator, Optional, NamedTuple


class EmptyListException(Exception):
    def __init__(self) -> None:
        super().__init__("The list is empty.")


class Node(NamedTuple):
    value_: int
    next_: Optional[Node]

    def value(self) -> int:
        return self.value_

    def next(self) -> Optional[Node]:
        return self.next_


class LinkedList:
    _head: Optional[Node]

    def __init__(self, values: Optional[Iterable[int]] = None):
        self._head = None
        if values is None:
            return
        for value in values:
            self.push(value)

    def __iter__(self) -> Iterator[int]:
        return copy(self)

    def __len__(self) -> int:
        return sum(1 for _ in self)

    def head(self) -> Node:
        if self._head is None:
            raise EmptyListException
        return self._head

    def push(self, value: int) -> None:
        new_head = Node(value, self._head)
        self._head = new_head

    def pop(self) -> int:
        if self._head is None:
            raise EmptyListException
        value, new_head = self._head
        self._head = new_head
        return value

    def reversed(self) -> LinkedList:
        r = LinkedList()
        for x in self:
            r.push(x)
        return r

    def __next__(self) -> int:
        if self._head is None:
            raise StopIteration
        value, self._head = self._head
        return value
