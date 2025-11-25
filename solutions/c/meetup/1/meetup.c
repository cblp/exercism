#include "meetup.h"

#include <assert.h>
#include <stdio.h>
#include <time.h>

static int parse_day_of_week(const char* const day_of_week) {
    // f([x, y]) = (3 * y - x) % 9
    static int table[] = {
        3,  // f("We") = 0
        6,  // f("Sa") = 1
        5,  // f("Fr") = 2
        4,  // f("Th") = 3
        1,  // f("Mo") = 4
        -1,
        2,  // f("Tu") = 6
        0,  // f("Su") = 7
        -1,
    };
    return table[(3 * day_of_week[1] - day_of_week[0]) % 9];
}

typedef enum {
    first,
    second,
    third,
    fourth,
    teenth,
    last,
} week_selector_t;

static week_selector_t parse_week_selector(
    const char* const week_selector_str) {
    // f([x, y]) = (3 * x + y) % 9
    static int table[] = {
        -1,     -1,
        third,   // f("th") = 2
        fourth,  // f("fo") = 3
        -1,
        second,  // f("se") = 5
        first,   // f("fi") = 6
        last,    // f("la") = 7
        teenth,  // f("te") = 8
    };
    return table[(3 * week_selector_str[0] + week_selector_str[1]) % 9];
}

int meetup_day_of_month(unsigned const year, unsigned const target_month,
                        const char* const week_selector_str,
                        const char* const day_of_week) {
    int const target_wday = parse_day_of_week(day_of_week);
    week_selector_t const week_selector =
        parse_week_selector(week_selector_str);

    struct tm date = {0};
    date.tm_year = year - 1900;
    date.tm_mon = target_month - 1;

    static int mday_start[] = {
        /* first */ 1,
        /* second */ 8,
        /* third */ 15,
        /* fourth */ 22,
        /* teenth */ 13,
        /* last */ 22,
    };

    int mday = mday_start[week_selector];
    int mday_result = mday;  // update when searching for the last
    for (; mday <= 31; ++mday) {
        date.tm_mday = mday;
        mktime(&date);
        if (date.tm_mon != (int)target_month - 1) break;
        if (date.tm_wday != target_wday) continue;
        if (week_selector != last) return mday;
        mday_result = mday;
    }

    return mday_result;
}
