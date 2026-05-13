# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from calendar import Calendar
from calendar import (  # pylint: disable=no-name-in-module
    WEDNESDAY,
    FRIDAY,
    SATURDAY,
    SUNDAY,
)
from datetime import datetime, timedelta, time


def delivery_date(start: str, description: str) -> str:
    start_dt = datetime.fromisoformat(start)
    if description.endswith("M"):
        result = _month(start_dt, int(description[:-1]))
    elif description.startswith("Q"):
        result = _quarter(start_dt, int(description[1:]))
    else:
        result = {"NOW": _now, "ASAP": _asap, "EOW": _end_of_week}[description](
            start_dt
        )
    return result.isoformat()


def _now(start: datetime) -> datetime:
    return start + timedelta(hours=2)


ASAP_THRESHOLD_HOUR = 13


def _asap(start: datetime) -> datetime:
    return (
        datetime.combine(start.date(), time(17))
        if start.hour < ASAP_THRESHOLD_HOUR
        else datetime.combine(
            start.date() + timedelta(days=1), time(ASAP_THRESHOLD_HOUR)
        )
    )


def _end_of_week(start: datetime) -> datetime:
    weekday = start.weekday()
    return (
        datetime.combine(
            start.date() + timedelta(days=FRIDAY - weekday), time(17)
        )
        if weekday <= WEDNESDAY
        else datetime.combine(
            start.date() + timedelta(days=SUNDAY - weekday), time(20)
        )
    )


_CALENDAR = Calendar()


def _month(start: datetime, n: int) -> datetime:
    year = start.year if start.month < n else start.year + 1
    month = n
    day = next(
        day
        for day, day_of_week in _CALENDAR.itermonthdays2(year, month)
        if day and day_of_week < SATURDAY
    )
    return datetime(year, month, day, 8)


def _quarter(start: datetime, n: int) -> datetime:
    year = start.year if start.month <= 3 * n else start.year + 1
    month = n * 3
    day = next(
        day
        for day, day_of_week in reversed(
            list(_CALENDAR.itermonthdays2(year, month))
        )
        if day and day_of_week < SATURDAY
    )
    return datetime(year, month, day, 8)
