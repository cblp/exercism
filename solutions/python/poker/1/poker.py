# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from collections import Counter
from enum import IntEnum
from typing import NamedTuple


def best_hands(hands: list[str]) -> list[str]:
    return max_set(hands)


class Card(NamedTuple):
    value: int
    suit: str


Hand = list[Card]

Category = IntEnum(
    "Category",
    "Values OnePair TwoPairs ThreeOfAKind Straight Flush FullHouse FourOfAKind"
    " StraightFlush",
)


class Score(NamedTuple):
    category: Category
    values: list[int]


def parse_hand(hand: str) -> Hand:
    return [parse_card(card) for card in hand.split()]


def parse_card(card: str) -> Card:
    match list(card):
        case "A", s:
            return Card(14, s)
        case "K", s:
            return Card(13, s)
        case "Q", s:
            return Card(12, s)
        case "J", s:
            return Card(11, s)
        case "1", "0", s:
            return Card(10, s)
        case v, s:
            return Card(int(v), s)
        case _:
            raise ValueError(card)


def score(hand: Hand) -> Score:
    is_flush = len(set(card.suit for card in hand)) == 1

    def make_straight(value: int) -> Score:
        return Score(
            Category.StraightFlush if is_flush else Category.Straight, [value]
        )

    values_down = sorted((card.value for card in hand), reverse=True)
    if values_down == [14, 5, 4, 3, 2]:
        return make_straight(5)
    if all(x - y == 1 for x, y in zip(values_down, values_down[1:])):
        return make_straight(values_down[0])
    if is_flush:
        return Score(Category.Flush, values_down)
    values_by_frequency_down: list[tuple[int, int]] = sorted(
        (
            (count, value)
            for value, count in Counter(card.value for card in hand).items()
        ),
        reverse=True,
    )
    frequencies = [freq for freq, _value in values_by_frequency_down]
    match frequencies:
        case 4, 1:
            cat = Category.FourOfAKind
        case 3, 2:
            cat = Category.FullHouse
        case 3, 1, 1:
            cat = Category.ThreeOfAKind
        case 2, 2, 1:
            cat = Category.TwoPairs
        case 2, 1, 1, 1:
            cat = Category.OnePair
        case 1, 1, 1, 1, 1:
            cat = Category.Values
        case _:
            raise ValueError(values_by_frequency_down)
    return Score(cat, [value for _freq, value in values_by_frequency_down])


def max_set(items: list[str]) -> list[str]:
    first = items[0]
    maxkey = score(parse_hand(first))
    maxset = [first]
    for x in items[1:]:
        k = score(parse_hand(x))
        if k > maxkey:
            maxkey = k
            maxset = [x]
        elif k == maxkey:
            maxset.append(x)
    return maxset
