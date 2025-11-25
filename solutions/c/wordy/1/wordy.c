#include "wordy.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>

typedef struct slice_t {
    const char* begin;
    const char* end;
} slice_t;

slice_t find_last_word(slice_t const words) {
    for (const char* p = words.end; p > words.begin; p--) {
        if (*(p - 1) == ' ') return (slice_t){p, words.end};
    }
    return words;
}

static bool eval_number(slice_t const word, int* const result) {
    int r = 0;
    bool minus = false;
    for (const char* c = word.begin; c < word.end; c++) {
        if (*c == '-') {
            minus = true;
        } else if (isdigit(*c)) {
            r = r * 10 + (*c - '0');
        } else {
            return false;
        }
    }
    *result = minus ? -r : r;
    return true;
}

static bool slice_eq(slice_t const slice, const char* str) {
    return strncmp(slice.begin, str, slice.end - slice.begin) == 0;
}

static bool eval(slice_t const words, int* const result) {
    slice_t const z = find_last_word(words);
    if (z.begin == words.begin)  // only 1 word in words
    {
        return eval_number(z, result);
    }

    slice_t const before_z = {words.begin, z.begin - 1};
    slice_t const y = find_last_word(before_z);
    slice_t const before_y = {words.begin, y.begin - 1};
    if (slice_eq(y, "plus")) {
        int a, b;
        if (eval(before_y, &a) && eval_number(z, &b)) {
            *result = a + b;
            return true;
        }
        return false;
    } else if (slice_eq(y, "minus")) {
        int a, b;
        if (eval(before_y, &a) && eval_number(z, &b)) {
            *result = a - b;
            return true;
        }
        return false;
    }

    slice_t const x = find_last_word(before_y);
    slice_t const before_x = {words.begin, x.begin - 1};
    if (slice_eq((slice_t){x.begin, y.end}, "multiplied by")) {
        int a, b;
        if (eval(before_x, &a) && eval_number(z, &b)) {
            *result = a * b;
            return true;
        }
        return false;
    } else if (slice_eq((slice_t){x.begin, y.end}, "divided by")) {
        int a, b;
        if (eval(before_x, &a) && eval_number(z, &b) && b != 0) {
            *result = a / b;
            return true;
        }
        return false;
    }

    return false;
}

bool answer(const char* const question, int* const result) {
    assert(question);
    assert(result);
    size_t len = strlen(question);
    if (strncmp(question, "What is ", 8) != 0 || question[len - 1] != '?') {
        return false;
    }
    return eval((slice_t){question + 8, question + len - 1}, result);
}
