#include "say.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

enum {
    THOUSAND = 1000,
    MILLION = 1000 * THOUSAND,
    BILLION = 1000 * MILLION,
};

const char* const ONES[] = {
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
};

const char* const TENS[] = {
    "ten",   "twenty",  "thirty", "forty",  "fifty",
    "sixty", "seventy", "eighty", "ninety",
};

const char* const TEENS[] = {
    "eleven",  "twelve",    "thirteen", "fourteen", "fifteen",
    "sixteen", "seventeen", "eighteen", "nineteen",
};

static void say_word(const char* word, char* ans) {
    if (*ans) strcat(ans, " ");
    strcat(ans, word);
}

static void say_group(int64_t n, char* ans) {
    uint8_t const d0 = n % 10;
    uint8_t const d1 = (n / 10) % 10;
    uint8_t const d2 = n / 100;
    if (d2) {
        say_word(ONES[d2 - 1], ans);
        say_word("hundred", ans);
    }
    if (d0 == 0 && d1 == 0) {
        // skip
    } else if (d0 == 0) {
        say_word(TENS[d1 - 1], ans);
    } else if (d1 == 0) {
        say_word(ONES[d0 - 1], ans);
    } else if (d1 == 1) {
        say_word(TEENS[d0 - 1], ans);
    } else {
        say_word(TENS[d1 - 1], ans);
        strcat(ans, "-");
        strcat(ans, ONES[d0 - 1]);
    }
}

int say(int64_t input, char** ans) {
    assert(ans);
    if (input < 0 || input > 999999999999) {
        return -1;
    }
    if (input == 0) {
        *ans = malloc(5);
        strcpy(*ans, "zero");
        return 0;
    }

    *ans = malloc(128);
    **ans = 0;

    if (input >= BILLION) {
        say_group(input / BILLION, *ans);
        say_word("billion", *ans);
    }
    if ((input % BILLION) >= MILLION) {
        say_group((input % BILLION) / MILLION, *ans);
        say_word("million", *ans);
    }
    if ((input % MILLION) >= THOUSAND) {
        say_group((input % MILLION) / THOUSAND, *ans);
        say_word("thousand", *ans);
    }
    if (input % THOUSAND) {
        say_group(input % THOUSAND, *ans);
    }

    return 0;
}
