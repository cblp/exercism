# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


from enum import Flag

Allergy = Flag(
    "Allergy",
    "eggs peanuts shellfish strawberries tomatoes chocolate pollen cats",
)


class Allergies:
    # pylint: disable=too-few-public-methods

    def __init__(self, score: int):
        self.flags = {allergy for allergy in Allergy if score & allergy.value}
        self.lst = [str(allergy.name) for allergy in self.flags]

    def allergic_to(self, item: str) -> bool:
        return Allergy[item] in self.flags
