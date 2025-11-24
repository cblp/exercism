#include "word_count.h"

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// static void print_word_counters(const word_count_word_t* const words,
//                                 size_t const size) {
//     printf("[%zu]{\n", size);
//     for (size_t i = 0; i < size; ++i)
//         printf("  \"%s\": %d\n", words[i].text, words[i].count);
//     printf("}\n");
// }

static word_count_word_t* get_or_insert(word_count_word_t* counters,
                                        size_t* size, const char* word) {
    assert(counters);
    assert(size);
    assert(word);

    // binary search, assuming counters are sorted
    size_t inserting;
    {
        int left = 0;
        int right = *size - 1;
        while (left <= right) {
            int mid = left + (right - left) / 2;
            int cmp = strcmp(counters[mid].text, word);
            if (cmp == 0) {
                return counters + mid;
            } else if (cmp < 0) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        inserting = left;
    }
    // now left is the index where to insert
    memmove(counters + inserting + 1, counters + inserting,
            (*size - inserting) * sizeof(word_count_word_t));
    strcpy(counters[inserting].text, word);
    counters[inserting].count = 0;
    ++(*size);
    return counters + inserting;
}

int count_words(const char* sentence, word_count_word_t* words) {
    size_t unique_word_count = 0;

    char current_word[MAX_WORD_LENGTH + 1];
    size_t current_word_len = 0;

    bool apostrophe = false;

    for (; *sentence; ++sentence) {
        char c = *sentence;
        if (isalpha(c) || isdigit(c)) {
            if (apostrophe) {
                current_word[current_word_len++] = '\'';
                apostrophe = false;
            }
            current_word[current_word_len++] = tolower(c);
        } else if (c == '\'' && !apostrophe && current_word_len) {
            apostrophe = true;
        } else {
            apostrophe = false;
            if (current_word_len) {
                current_word[current_word_len] = 0;
                get_or_insert(words, &unique_word_count, current_word)->count++;
                current_word_len = 0;
                // print_word_counters(words, unique_word_count);
            }
        }
    }
    if (current_word_len) {
        current_word[current_word_len] = 0;
        get_or_insert(words, &unique_word_count, current_word)->count++;
        // print_word_counters(words, unique_word_count);
    }

    return unique_word_count;
}
