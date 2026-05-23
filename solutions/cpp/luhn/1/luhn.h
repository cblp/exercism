#pragma once

#include <string>

namespace luhn {

using namespace std;

inline bool valid(string input) {
    unsigned sum = 0;
    unsigned digits = 0;
    bool is_second = false;
    for (auto i = input.crbegin(); i != input.crend(); i++) {
        if (isspace(*i)) {
            continue;
        }
        if (not isdigit(*i)) {
            return false;
        }
        unsigned d = *i - '0';
        if (is_second) {
            d *= 2;
            if (d > 9) {
                d -= 9;
            }
        }
        is_second = not is_second;
        sum += d;
        digits++;
    }
    return digits > 1 and sum % 10 == 0;
}

}  // namespace luhn
