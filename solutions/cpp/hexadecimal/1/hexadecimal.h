#pragma once

#include <string>

namespace hexadecimal {

using namespace std;

inline unsigned convert(string digits) {
    unsigned r = 0;
    for (char c : digits) {
        r *= 16;
        if ('0' <= c and c <= '9') {
            r += c - '0';
        } else if ('a' <= c and c <= 'f') {
            r += c - ('a' - 10);
        } else if ('A' <= c and c <= 'F') {
            r += c - ('A' - 10);
        } else {
            return 0;
        }
    }
    return r;
}

}  // namespace hexadecimal
