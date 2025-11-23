#include "clock.h"

#include <stdio.h>
#include <string.h>

int mod(int x, int m);
int mod(int x, int m) { return (x % m + m) % m; }

int div_mod(int x, int m, int* div, int* mod);
int div_mod(int x, int m, int* div, int* mod) {
    if (x < 0) {
        *div = (x + 1) / m - 1;
        *mod = x - (*div) * m;
    } else {
        *div = x / m;
        *mod = x % m;
    }
    return 0;
}

clock_t clock_create(int hours, int minutes) {
    div_mod(mod(hours * 60 + minutes, 24 * 60), 60, &hours, &minutes);

    clock_t clock;
    snprintf(clock.text, MAX_STR_LEN, "%02d:%02d", hours, minutes);
    return clock;
}

int clock_hours(clock_t c);
int clock_hours(clock_t c) {
    return (c.text[0] - '0') * 10 + (c.text[1] - '0');
}

int clock_minutes(clock_t c);
int clock_minutes(clock_t c) {
    return (c.text[3] - '0') * 10 + (c.text[4] - '0');
}

clock_t clock_add(clock_t c, int const minutes_to_add) {
    return clock_create(clock_hours(c), clock_minutes(c) + minutes_to_add);
}

clock_t clock_subtract(clock_t clock, int minutes_to_subtract) {
    return clock_add(clock, -minutes_to_subtract);
}

bool clock_is_equal(clock_t a, clock_t b) {
    return strncmp(a.text, b.text, MAX_STR_LEN) == 0;
}
