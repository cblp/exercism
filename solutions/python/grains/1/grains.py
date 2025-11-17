def square(number):
    if number == 1:
        return 1
    elif 1 < number <= 64:
        return 2 * square(number - 1)
    else:
        raise ValueError("square must be between 1 and 64")


def total():
    return sum(square(number) for number in range(1, 65))
