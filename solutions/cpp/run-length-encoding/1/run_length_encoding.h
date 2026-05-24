#pragma once

#include <string>

namespace run_length_encoding {

using namespace std;

inline string encode(string text) {
    string r;
    int n = 0;
    char k = 0;

    const auto append = [&]() {
        if (not k) {
            return;
        }
        if (n != 1) {
            r += to_string(n);
        }
        r += k;
    };

    for (char c : text) {
        if (c == k) {
            n++;
        } else {
            append();
            n = 1;
            k = c;
        }
    }
    append();
    return r;
}

inline string decode(string text) {
    string r;
    int n = 0;
    for (char c : text) {
        if (isdigit(c)) {
            n = n * 10 + (c - '0');
        } else {
            r += string(max(1, n), c);
            n = 0;
        }
    }
    return r;
}

}  // namespace run_length_encoding
