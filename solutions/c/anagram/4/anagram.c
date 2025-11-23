#include "anagram.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static int compare_char(const void* a, const void* b) {
    return (*(const char*)a) - (*(const char*)b);
}

static char* strdup_(const char* const s) {
    size_t const len = strlen(s);
    char* const copy = malloc(len + 1);
    if (copy) memcpy(copy, s, len + 1);
    return copy;
}

static int strcasecmp_(const char* s1, const char* s2) {
    for (; *s1 && *s2; ++s1, ++s2) {
        char const c1 = tolower(*s1);
        char const c2 = tolower(*s2);
        if (c1 != c2) return c1 - c2;
    }
    return *s1 - *s2;
}

void find_anagrams(const char* const subject_orig,
                   struct candidates* const candidates) {
    char* const subject_buf = strdup_(subject_orig);
    for (char* p = subject_buf; *p; ++p) *p = tolower(*p);
    size_t const len = strlen(subject_buf);
    qsort(subject_buf, len, 1, compare_char);
    for (size_t i = 0; i < candidates->count; ++i) {
        struct candidate* const cand = &candidates->candidate[i];
        if (strlen(cand->word) != len) {
            cand->is_anagram = false;
            continue;
        }
        char* const word_buf = strdup_(cand->word);
        for (char* p = word_buf; *p; ++p) *p = tolower(*p);
        qsort(word_buf, len, 1, compare_char);
        cand->is_anagram = (strcmp(subject_buf, word_buf) == 0 &&
                            strcasecmp_(subject_orig, cand->word) != 0);
        free(word_buf);
    }
    free(subject_buf);
}
