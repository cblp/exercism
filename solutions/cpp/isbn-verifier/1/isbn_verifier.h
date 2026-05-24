#pragma once

#include <string>

namespace isbn_verifier {

inline bool is_valid(std::string input) {
    int s = 0;
    int n = 0;
    for (char c : input) {
        if (c == '-') {
            continue;
        }
        if (isdigit(c)) {
            s += (c - '0') * (10 - n);
        } else if (c == 'X') {
            s += 10;
        } else {
            return false;
        }
        n++;
    }
    return n == 10 and s % 11 == 0;
}

}  // namespace isbn_verifier
