#include "clock.h"

#include <stdio.h>
#include <string.h>

enum ClockDigitPosition {
    HOURS_TENS,
    HOURS_ONES,
    SEMICOLON,
    MINUTES_TENS,
    MINUTES_ONES,
};

int div(int x, int m);
int div(int x, int m) { return x < 0 ? (x + 1) / m - 1 : x / m; }

int mod(int x, int m);
int mod(int x, int m) { return (x % m + m) % m; }

clock_t clock_create(int hours, int minutes) {
    hours += div(minutes, 60);
    minutes = mod(minutes, 60);
    hours = mod(hours, 24);

    clock_t clock;
    snprintf(clock.text, MAX_STR_LEN, "%02d:%02d", hours, minutes);
    return clock;
}

clock_t clock_add(clock_t clock, int const minutes_to_add) {
    int minutes = (clock.text[MINUTES_TENS] - '0') * 10 +
                  (clock.text[MINUTES_ONES] - '0');
    minutes += minutes_to_add;
    int hours_to_add = div(minutes, 60);
    minutes = mod(minutes, 60);

    int hours =
        (clock.text[HOURS_TENS] - '0') * 10 + (clock.text[HOURS_ONES] - '0');
    hours += hours_to_add;
    hours = mod(hours, 24);

    return clock_create(hours, minutes);
}

clock_t clock_subtract(clock_t clock, int minutes_to_subtract) {
    return clock_add(clock, -minutes_to_subtract);
}

bool clock_is_equal(clock_t a, clock_t b) {
    return strncmp(a.text, b.text, MAX_STR_LEN) == 0;
}
