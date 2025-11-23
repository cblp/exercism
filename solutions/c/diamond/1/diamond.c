#include "diamond.h"

#include <assert.h>
#include <stdlib.h>

char** make_diamond(const char letter) {
    unsigned level = letter - 'A';
    unsigned size = level * 2 + 1;
    char** diamond = malloc(size * sizeof(char*));
    assert(diamond);
    for (unsigned i = 0; i < size; ++i) {
        diamond[i] = malloc((size + 1) * sizeof(char));
        assert(diamond[i]);
        for (unsigned j = 0; j < size; ++j) {
            diamond[i][j] = ' ';
        }
        diamond[i][size] = '\0';
    }
    for (unsigned i = 0; i <= level; ++i) {
        char ch = 'A' + i;
        diamond[i][level - i] = ch;
        diamond[i][level + i] = ch;
        diamond[size - 1 - i][level - i] = ch;
        diamond[size - 1 - i][level + i] = ch;
    }
    return diamond;
}

void free_diamond(char** diamond) { free(diamond); }
