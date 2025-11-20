#include "isogram.h"

#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>

bool is_isogram(const char phrase[]) {
    if (!phrase) {
        return false;
    }

    bool letter_flags[26] = {0};
    for (const char* c = phrase; *c; ++c) {
        if (isalpha(*c)) {
            uint8_t n = tolower(*c) - 'a';
            if (letter_flags[n]) {
                return false;
            }
            letter_flags[n] = true;
        }
    }
    return true;
}
