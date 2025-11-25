#include "pig_latin.h"

#include "assert.h"
#include "stdbool.h"
#include "stdlib.h"
#include "string.h"

#define MAX_RESULT 30

typedef struct {
    char* data;
    size_t size;
} stringbuf_t;

static stringbuf_t stringbuf_new(void) {
    stringbuf_t s = {0};
    s.data = calloc(1, MAX_RESULT);
    return s;
}

static void stringbuf_push(stringbuf_t* const s, char const c) {
    assert(s->size < MAX_RESULT);
    s->data[s->size++] = c;
}

static size_t stringview_length(const char* const begin,
                                const char* const end) {
    assert(begin <= end);
    return end - begin;
}

static void stringbuf_push_slice(stringbuf_t* const out,
                                 const char* const in_begin,
                                 const char* const in_end) {
    size_t const in_len = stringview_length(in_begin, in_end);
    assert(out->size + in_len <= MAX_RESULT);
    memcpy(out->data + out->size, in_begin, in_len);
    out->size += in_len;
}

static void stringbuf_push_stringz(stringbuf_t* const s, const char* c) {
    for (; *c; c++) stringbuf_push(s, *c);
}

static bool is_vowel(char const c) {
    return c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u';
}

static bool is_vowel_or_y(char const c) { return is_vowel(c) || c == 'y'; }

static void translate_word(const char* const word_begin,
                           const char* const word_end, stringbuf_t* const out) {
    // rule 1
    if (is_vowel(*word_begin) || strncmp(word_begin, "xr", 2) == 0 ||
        strncmp(word_begin, "yt", 2) == 0) {
        stringbuf_push_slice(out, word_begin, word_end);
        stringbuf_push_stringz(out, "ay");
        return;
    }

    // rules 2 and 4
    size_t consonants_count = 0;
    if (*word_begin == 'y') consonants_count++;
    while (!is_vowel_or_y(word_begin[consonants_count])) consonants_count++;

    if (consonants_count > 0) {
        // rule 3
        if (strncmp(word_begin + consonants_count - 1, "qu", 2) == 0)
            consonants_count++;

        // back to rule 2
        stringbuf_push_slice(out, word_begin + consonants_count, word_end);
        stringbuf_push_slice(out, word_begin, word_begin + consonants_count);
        stringbuf_push_stringz(out, "ay");
        return;
    }

    // fallback
    stringbuf_push_slice(out, word_begin, word_end);
}

char* translate(const char* const phrase) {
    assert(phrase);
    stringbuf_t out = stringbuf_new();
    size_t word_begin = 0;
    for (size_t i = 0; phrase[i]; i++) {
        if (phrase[i] == ' ') {
            translate_word(phrase + word_begin, phrase + i, &out);
            stringbuf_push(&out, ' ');
            i++;
            word_begin = i;
        } else if (phrase[i + 1] == 0)  // end case
        {
            translate_word(phrase + word_begin, phrase + i + 1, &out);
        }
    }
    stringbuf_push(&out, 0);
    return out.data;
}
