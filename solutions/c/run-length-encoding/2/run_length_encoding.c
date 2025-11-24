#include "run_length_encoding.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_STRING 100

typedef struct {
    char* const data;
    size_t size;
    size_t const capacity;
} string_t;

static string_t string_new(size_t capacity) {
    return (string_t){
        .data = malloc(capacity),
        .size = 0,
        .capacity = capacity,
    };
}

static void string_push(string_t* v, char value) {
    assert(v->size < v->capacity);
    v->data[v->size++] = value;
}

static void string_write_num(string_t* v, size_t n) {
    int const r = snprintf(v->data + v->size, v->capacity - v->size, "%zu", n);
    assert(r >= 0);
    v->size = v->size + r <= v->capacity ? v->size + r : v->capacity;
}

char* encode(const char* text) {
    assert(text);
    string_t out = string_new(MAX_STRING);
    size_t count = 0;
    char c = 0;
    for (; *text; ++text) {
        if (count == 0) {
            c = *text;
            count = 1;
        } else if (*text == c) {
            ++count;
        } else {
            // flush
            if (count > 1) string_write_num(&out, count);
            string_push(&out, c);

            c = *text;
            count = 1;
        }
    }
    if (count) {
        // flush
        if (count > 1) string_write_num(&out, count);
        string_push(&out, c);
    }
    string_push(&out, 0);
    return out.data;
}

char* decode(const char* data) {
    assert(data);
    string_t out = string_new(MAX_STRING);
    size_t n = 0;
    for (; *data; ++data) {
        char const c = *data;
        if (isdigit(c)) {
            n = n * 10 + (c - '0');
        } else {
            if (n == 0) n = 1;
            for (size_t i = 0; i < n; ++i) string_push(&out, c);
            n = 0;
        }
    }
    string_push(&out, 0);
    return out.data;
}
