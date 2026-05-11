# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


class PhoneNumber:
    area_code: str
    exchange: str
    subscriber: str

    def __init__(self, number_raw: str):
        number_clean = ""
        for char in number_raw:
            if char.isalpha():
                raise ValueError("letters not permitted")
            if char.isdigit():
                number_clean += char
            elif char not in "+-() .":
                raise ValueError("punctuations not permitted")
        if len(number_clean) > 11:
            raise ValueError("must not be greater than 11 digits")
        if len(number_clean) < 10:
            raise ValueError("must not be fewer than 10 digits")
        if len(number_clean) == 11:
            if number_clean[0] != "1":
                raise ValueError("11 digits must start with 1")
            number_clean = number_clean[1:]
        if number_clean[0] < "2":
            raise ValueError(
                "area code cannot start with "
                + ("one" if number_clean[0] == "1" else "zero")
            )
        if number_clean[3] < "2":
            raise ValueError(
                "exchange code cannot start with "
                + ("one" if number_clean[3] == "1" else "zero")
            )

        self.area_code = number_clean[:3]
        self.exchange = number_clean[3:6]
        self.subscriber = number_clean[6:]

    @property
    def number(self) -> str:
        return f"{self.area_code}{self.exchange}{self.subscriber}"

    def pretty(self) -> str:
        return f"({self.area_code})-{self.exchange}-{self.subscriber}"
