#include "anagram.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static int compare_char(const void* a, const void* b) {
    return (*(const char*)a) - (*(const char*)b);
}

void find_anagrams(const char* const subject_orig,
                   struct candidates* const candidates) {
    char* subject_buf = strdup(subject_orig);
    for (char* p = subject_buf; *p; ++p) *p = tolower(*p);
    size_t len = strlen(subject_buf);
    qsort(subject_buf, len, 1, compare_char);
    for (size_t i = 0; i < candidates->count; ++i) {
        struct candidate* cand = &candidates->candidate[i];
        if (strlen(cand->word) != len) {
            cand->is_anagram = false;
            continue;
        }
        char* word_buf = strdup(cand->word);
        for (char* p = word_buf; *p; ++p) *p = tolower(*p);
        qsort(word_buf, len, 1, compare_char);
        cand->is_anagram = (strcmp(subject_buf, word_buf) == 0 &&
                            strcasecmp(subject_orig, cand->word) != 0);
        free(word_buf);
    }
    free(subject_buf);
}
