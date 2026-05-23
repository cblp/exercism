#pragma once

#include <string>

namespace acronym {

using namespace std;

inline string acronym(string words) {
    string r;
    bool first_letter_found = false;
    for (char c : words) {
        if (c == ' ' or c == '-') {
            first_letter_found = false;
        } else if (isalpha(c)) {
            if (!first_letter_found) {
                r += toupper(c);
                first_letter_found = true;
            }
        }
    }
    return r;
}

}  // namespace acronym
