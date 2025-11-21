#include "rotational_cipher.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

char rotate_letter(char c, char base, int key);
char rotate_letter(char c, char base, int key) {
    return (c - base + key) % 26 + base;
}

char rotate_char(char c, int key);
char rotate_char(char c, int key) {
    return isupper(c)   ? rotate_letter(c, 'A', key)
           : islower(c) ? rotate_letter(c, 'a', key)
                        : c;
}

char* rotate(const char* text, int key) {
    size_t const n = strlen(text);
    char* out = malloc(n + 1);
    for (size_t i = 0; i <= n; ++i) {
        out[i] = rotate_char(text[i], key);
    }
    return out;
}
