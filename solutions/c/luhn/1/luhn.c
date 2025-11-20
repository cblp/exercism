#include "luhn.h"

#include <ctype.h>

bool luhn(const char* num) {
    // find the last char
    const char* p = num;
    while (*p) ++p;
    --p;

    int checksum = 0;
    int digits = 0;
    for (; p >= num; --p) {
        if (isspace(*p)) continue;
        if (!isdigit(*p)) return false;
        digits += 1;
        char digit = *p - '0';
        if (digits % 2 == 0) {
            digit *= 2;
            if (digit > 9) digit -= 9;
        }
        checksum += digit;
    }
    return digits >= 2 && checksum % 10 == 0;
}
