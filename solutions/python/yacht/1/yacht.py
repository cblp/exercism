# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


from enum import Enum, global_enum

Category = global_enum(
    Enum(
        "Category",
        [
            "ONES",
            "TWOS",
            "THREES",
            "FOURS",
            "FIVES",
            "SIXES",
            "FULL_HOUSE",
            "FOUR_OF_A_KIND",
            "LITTLE_STRAIGHT",
            "BIG_STRAIGHT",
            "CHOICE",
            "YACHT",
        ],
    )
)


def score(dice: list[int], category: Category) -> int:
    dice = sorted(dice)

    return (
        {
            cat: sum(x for x in dice if x == cat.value)
            for cat in [ONES, TWOS, THREES, FOURS, FIVES, SIXES]
        }
        | {
            FULL_HOUSE: (
                sum(dice) * (sorted(dice.count(v) for v in set(dice)) == [2, 3])
            ),
            FOUR_OF_A_KIND: next(
                (x * 4 for x in set(dice) if dice.count(x) >= 4), 0
            ),
            LITTLE_STRAIGHT: 30 * (dice == [1, 2, 3, 4, 5]),
            BIG_STRAIGHT: 30 * (dice == [2, 3, 4, 5, 6]),
            CHOICE: sum(dice),
            YACHT: 50 * (len(set(dice)) == 1),
        }
    ).get(category, 0)
