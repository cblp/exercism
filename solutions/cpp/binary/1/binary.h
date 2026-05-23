#pragma once

#include <string>

namespace binary {

using namespace std;

inline unsigned convert(string digits) {
    unsigned r = 0;
    for (char c : digits) {
        if (c != '0' and c != '1') {
            return 0;
        }
        r = r * 2 + (c - '0');
    }
    return r;
}

}  // namespace binary
