from itertools import islice
from random import randint


def d6() -> int:
    return randint(1, 6)


def random_ability() -> int:
    return sum(islice(sorted((d6() for _ in range(4)), reverse=True), 3))


class Character:
    strength: int
    dexterity: int
    constitution: int
    intelligence: int
    wisdom: int
    charisma: int

    hitpoints: int

    def __init__(self) -> None:
        self.strength = random_ability()
        self.dexterity = random_ability()
        self.constitution = random_ability()
        self.intelligence = random_ability()
        self.wisdom = random_ability()
        self.charisma = random_ability()
        self.hitpoints = 10 + modifier(self.constitution)

    @staticmethod
    def ability() -> int:
        return random_ability()


def modifier(value: int) -> int:
    return (value - 10) // 2
