#include "pangram.h"

#include <ctype.h>

bool is_pangram(const char* sentence) {
    if (!sentence) return false;

    bool letter_flags[26] = {0};
    for (const char* c = sentence; *c; ++c) {
        if (isalpha(*c)) letter_flags[tolower(*c) - 'a'] = true;
    }

    for (size_t i = 0; i < 26; ++i)
        if (!letter_flags[i]) return false;
    return true;
}
