#pragma once

#include <boost/date_time/gregorian/gregorian_types.hpp>

namespace meetup {

using namespace boost::gregorian;

inline int mod(int a, int n) {
    const int r = a % n;
    return r < 0 ? r + n : r;
}

struct scheduler {
    greg_month month;
    greg_year year;

    date first_monday() const { return first_from(1, Monday); }
    date first_tuesday() const { return first_from(1, Tuesday); }
    date first_wednesday() const { return first_from(1, Wednesday); }
    date first_thursday() const { return first_from(1, Thursday); }
    date first_friday() const { return first_from(1, Friday); }
    date first_saturday() const { return first_from(1, Saturday); }
    date first_sunday() const { return first_from(1, Sunday); }

    date second_monday() const { return first_from(8, Monday); }
    date second_tuesday() const { return first_from(8, Tuesday); }
    date second_wednesday() const { return first_from(8, Wednesday); }
    date second_thursday() const { return first_from(8, Thursday); }
    date second_friday() const { return first_from(8, Friday); }
    date second_saturday() const { return first_from(8, Saturday); }
    date second_sunday() const { return first_from(8, Sunday); }

    date monteenth() const { return first_from(13, Monday); }
    date tuesteenth() const { return first_from(13, Tuesday); }
    date wednesteenth() const { return first_from(13, Wednesday); }
    date thursteenth() const { return first_from(13, Thursday); }
    date friteenth() const { return first_from(13, Friday); }
    date saturteenth() const { return first_from(13, Saturday); }
    date sunteenth() const { return first_from(13, Sunday); }

    date third_monday() const { return first_from(15, Monday); }
    date third_tuesday() const { return first_from(15, Tuesday); }
    date third_wednesday() const { return first_from(15, Wednesday); }
    date third_thursday() const { return first_from(15, Thursday); }
    date third_friday() const { return first_from(15, Friday); }
    date third_saturday() const { return first_from(15, Saturday); }
    date third_sunday() const { return first_from(15, Sunday); }

    date fourth_monday() const { return first_from(22, Monday); }
    date fourth_tuesday() const { return first_from(22, Tuesday); }
    date fourth_wednesday() const { return first_from(22, Wednesday); }
    date fourth_thursday() const { return first_from(22, Thursday); }
    date fourth_friday() const { return first_from(22, Friday); }
    date fourth_saturday() const { return first_from(22, Saturday); }
    date fourth_sunday() const { return first_from(22, Sunday); }

    date last_monday() const { return last(Monday); }
    date last_tuesday() const { return last(Tuesday); }
    date last_wednesday() const { return last(Wednesday); }
    date last_thursday() const { return last(Thursday); }
    date last_friday() const { return last(Friday); }
    date last_saturday() const { return last(Saturday); }
    date last_sunday() const { return last(Sunday); }

   private:
    date first_from(greg_day start, greg_weekday weekday) const {
        date d{year, month, start};
        return d + date_duration(mod(weekday - d.day_of_week(), 7));
    }

    date last(greg_weekday weekday) const {
        return first_from(gregorian_calendar::end_of_month_day(year, month) - 6,
                          weekday);
    }
};

}  // namespace meetup
