#include "bob.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void trim_space_right(char* str) {
    if (!str) return;
    char* end = str;
    while (*end) end++;
    end--;
    while (end >= str && isspace(*end)) {
        *end = 0;
        end--;
    }
}

static char* strdup_(const char* const s) {
    size_t const len = strlen(s);
    char* const copy = malloc(len + 1);
    if (copy) memcpy(copy, s, len + 1);
    return copy;
}

const char* hey_bob(const char* greeting_original) {
    char* greeting = strdup_(greeting_original);
    trim_space_right(greeting);
    size_t const len = strlen(greeting);

    bool has_letters = false;
    bool all_letters_are_upper = true;
    for (char* p = greeting; *p; p++) {
        if (isalpha(*p)) {
            has_letters = true;
            if (!isupper(*p)) {
                all_letters_are_upper = false;
            }
        }
    }
    bool const is_yell = has_letters && all_letters_are_upper;
    bool const is_silence = greeting[0] == '\0';
    bool const is_question = len && greeting[len - 1] == '?';

    free(greeting);

    if (is_yell && is_question) {
        return "Calm down, I know what I'm doing!";
    }
    if (is_question) {
        return "Sure.";
    }
    if (is_yell) {
        return "Whoa, chill out!";
    }
    if (is_silence) {
        return "Fine. Be that way!";
    }
    return "Whatever.";
}
