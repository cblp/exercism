from calendar import Calendar
from datetime import date


# subclassing the built-in ValueError to create MeetupDayException
class MeetupDayException(ValueError):
    """Exception raised when the Meetup weekday and count do not result in a valid date.

    message: explanation of the error.
    """


def select_day(selector: str, days: list[int]) -> int:
    try:
        return {
            "first": lambda: days[0],
            "second": lambda: days[1],
            "third": lambda: days[2],
            "fourth": lambda: days[3],
            "fifth": lambda: days[4],
            "last": lambda: days[-1],
            "teenth": lambda: [d for d in days if 10 <= d < 20][-1],
        }[selector]()
    except IndexError as exc:
        raise MeetupDayException("That day does not exist.") from exc


def meetup(year: int, month: int, selector: str, day_of_week: str) -> date:
    target_weekday = {
        "Monday": 0,
        "Tuesday": 1,
        "Wednesday": 2,
        "Thursday": 3,
        "Friday": 4,
        "Saturday": 5,
        "Sunday": 6,
    }[day_of_week]
    days = [
        monthday
        for monthday, weekday in Calendar().itermonthdays2(year, month)
        if monthday and weekday == target_weekday
    ]
    day = select_day(selector, days)
    return date(year, month, day)
