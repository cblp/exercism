#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Category {
    Ones,
    Twos,
    Threes,
    Fours,
    Fives,
    Sixes,
    FullHouse,
    FourOfAKind,
    LittleStraight,
    BigStraight,
    Choice,
    Yacht,
}
use Category::*;

type Dice = [u8; 5];

pub fn score(mut dice: Dice, category: Category) -> u8 {
    dice.sort();

    let count = |x| dice.iter().filter(|y| x == **y).count();
    let value = category as u8 + 1;
    let [a, b, c, d, e] = dice;
    let eq = |it| dice.into_iter().eq(it);
    let sum = dice.iter().sum();
    let all_eq = |x| dice.iter().all(|y| x == *y);
    let sum_eq = |v| dice.iter().filter(|x| **x == v).sum();

    match category {
        _ if value <= 6 => sum_eq(value),
        FullHouse if a == b && (b == c || c == d) && b != e && d == e => sum,
        FourOfAKind if count(a) >= 4 => a * 4,
        FourOfAKind if count(b) >= 4 => b * 4,
        LittleStraight if eq(1..=5) => 30,
        BigStraight if eq(2..=6) => 30,
        Choice => sum,
        Yacht if all_eq(a) => 50,
        _ => 0,
    }
}
