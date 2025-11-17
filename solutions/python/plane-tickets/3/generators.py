"""Functions to automate Conda airlines ticketing system."""

from itertools import islice
from typing import Generator, Optional


def generate_seat_letters(number: int) -> Generator[str, None, None]:
    """Generate a series of letters for airline seats.

    :param number: int - total number of seat letters to be generated.
    :return: generator - generator that yields seat letters.

    Seat letters are generated from A to D.
    After D it should start again with A.

    Example: A, B, C, D
    """

    for i in range(number):
        yield "ABCD"[i % 4]


def generate_seats(number: Optional[int] = None) -> Generator[str, None, None]:
    """Generate a series of identifiers for airline seats.

    :param number: int - total number of seats to be generated.
    :return: generator - generator that yields seat numbers.

    A seat number consists of the row number and the seat letter.

    There is no row 13.
    Each row has 4 seats.

    Seats should be sorted from low to high.

    Example: 3C, 3D, 4A, 4B
    """

    if number is None:
        row = 1
        while True:
            if row != 13:
                for seat in "ABCD":
                    yield f"{row}{seat}"
            row += 1
    else:
        yield from islice(generate_seats(), None, number)


def assign_seats(passengers: list[str]) -> dict[str, str]:
    """Assign seats to passengers.

    :param passengers: list[str] - a list of strings containing names of passengers.
    :return: dict - with the names of the passengers as keys and seat numbers as values.

    Example output: {"Adele": "1A", "Björk": "1B"}
    """

    return dict(zip(passengers, generate_seats()))


def generate_codes(
    seat_numbers: list[str], flight_id: str
) -> Generator[str, None, None]:
    """Generate codes for a ticket.

    :param seat_numbers: list[str] - list of seat numbers.
    :param flight_id: str - string containing the flight identifier.
    :return: generator - generator that yields 12 character long ticket codes.
    """

    for seat in seat_numbers:
        code = f"{seat}{flight_id}"
        yield code + "0" * (12 - len(code))
