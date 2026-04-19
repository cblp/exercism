use {
    itertools::Itertools,
    std::{cmp::Reverse, iter::zip},
};

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings
/// which happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Vec<&'a str> {
    zip(hands.iter().copied().map(parse_hand), hands)
        .max_set_by_key(|(h, _)| score(h))
        .into_iter()
        .map(|(_, s)| s)
        .copied()
        .collect_vec()
}

type Card = (u8, char);

type Hand = Vec<Card>;

fn parse_hand(hand_str: &str) -> Hand {
    hand_str.split(' ').map(parse_card).collect_vec()
}

fn parse_card(card_str: &str) -> Card {
    match card_str.as_bytes() {
        [b'A', suit] => (14, *suit as char),
        [b'K', suit] => (13, *suit as char),
        [b'Q', suit] => (12, *suit as char),
        [b'J', suit] => (11, *suit as char),
        [b'1', b'0', suit] => (10, *suit as char),
        [digit, suit] => (digit - b'0', *suit as char),
        _ => panic!("{card_str:?}"),
    }
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Debug)]
enum Score {
    Values(Vec<u8>),
    OnePair {
        pair: u8,
        kickers: Vec<u8>,
    },
    TwoPairs {
        first_pair: u8,
        second_pair: u8,
        kicker: u8,
    },
    ThreeOfAKind {
        value: u8,
        kickers: Vec<u8>,
    },
    Straight(u8),
    Flush(Vec<u8>),
    FullHouse {
        triplet: u8,
        pair: u8,
    },
    FourOfAKind {
        value: u8,
        kicker: u8,
    },
    StraightFlush(u8),
}
use Score::*;

fn score(cards: &[Card]) -> Score {
    let values_down = cards
        .iter()
        .map(|(value, _)| *value)
        .sorted_by_key(|&x| Reverse(x))
        .collect_vec();
    let is_flush = cards.iter().map(|(_, suit)| suit).all_equal();
    let straight = |s| {
        if is_flush {
            StraightFlush(s)
        } else {
            Straight(s)
        }
    };
    if values_down == [14, 5, 4, 3, 2] {
        return straight(5);
    }
    if values_down.windows(2).all(|w| w[1] + 1 == w[0]) {
        return straight(values_down[0]);
    }
    if is_flush {
        return Flush(values_down);
    }
    let values_by_frequency_down: Vec<(usize, u8)> = cards
        .iter()
        .into_group_map_by(|(value, _)| *value)
        .into_iter()
        .map(|(value, cards)| (cards.len(), value))
        .sorted_by_key(|&x| Reverse(x))
        .collect_vec();
    match values_by_frequency_down[..] {
        [(4, value), (1, kicker)] => FourOfAKind { value, kicker },
        [(3, triplet), (2, pair)] => FullHouse { triplet, pair },
        [(3, value), ref kickers @ ..] => ThreeOfAKind {
            value,
            kickers: kickers.iter().map(|(_, value)| *value).collect_vec(),
        },
        [(2, first_pair), (2, second_pair), (1, kicker)] => TwoPairs {
            first_pair,
            second_pair,
            kicker,
        },
        [(2, pair), ref kickers @ ..] => OnePair {
            pair,
            kickers: kickers.iter().map(|(_, value)| *value).collect_vec(),
        },
        _ => Values(values_down),
    }
}
