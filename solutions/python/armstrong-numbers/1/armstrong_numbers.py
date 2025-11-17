def is_armstrong_number(number):
    digits = str(number)
    number_of_digits = len(digits)
    return number == sum(int(digit) ** number_of_digits for digit in digits)
