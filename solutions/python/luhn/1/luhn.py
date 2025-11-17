class Luhn:
    card_num: str

    def __init__(self, card_num: str):
        self.card_num = card_num

    def valid(self) -> bool:
        checksum = 0
        digits = 0
        for c in reversed(self.card_num):
            if c.isspace():
                continue
            if not c.isdecimal():
                return False
            digits += 1
            digit = ord(c) - ord("0")
            if digits % 2 == 0:
                digit *= 2
                if digit > 9:
                    digit -= 9
            checksum += digit
        return digits >= 2 and checksum % 10 == 0
