#pragma once

#include <string>

namespace reverse_string {

using namespace std;

inline string reverse_string(string s) {
    for (size_t i = 0; i < s.size() / 2; ++i) {
        swap(s[i], s[s.size() - 1 - i]);
    }
    return s;
}

}  // namespace reverse_string
