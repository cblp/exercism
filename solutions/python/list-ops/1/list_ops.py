from typing import Callable, TypeVar

A = TypeVar("A")
B = TypeVar("B")


def cons(a: A, ls: list[A]) -> list[A]:
    return [a] + ls


def elim(
    ls: list[A], if_nil: Callable[[], B], if_cons: Callable[[A, list[A]], B]
) -> B:
    return if_cons(ls[0], ls[1:]) if ls else if_nil()


def append(list1: list[A], list2: list[A]) -> list[A]:
    return elim(
        list1, lambda: list2, lambda head, tail: cons(head, append(tail, list2))
    )


def concat(lists: list[list[A]]) -> list[A]:
    return foldr(lambda acc, x: x + acc, lists, [])


def filter(function: Callable[[A], bool], ls: list[A]) -> list[A]:
    return foldr(lambda acc, x: cons(x, acc) if function(x) else acc, ls, [])


def length(ls: list[A]) -> int:
    return foldr(lambda acc, _: acc + 1, ls, 0)


def map(function: Callable[[A], B], ls: list[A]) -> list[B]:
    return foldr(lambda acc, x: cons(function(x), acc), ls, [])


def foldl(function: Callable[[B, A], B], ls: list[A], initial: B) -> B:
    return elim(
        ls,
        lambda: initial,
        lambda head, tail: foldl(function, tail, function(initial, head)),
    )


def foldr(function: Callable[[B, A], B], ls: list[A], initial: B) -> B:
    return elim(
        ls,
        lambda: initial,
        lambda head, tail: function(foldr(function, tail, initial), head),
    )


def reverse(ls: list[A]) -> list[A]:
    return foldl(lambda acc, x: cons(x, acc), ls, [])
