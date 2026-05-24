#pragma once

#include <stdexcept>
#include <string>
#include <vector>

namespace resistor_color {

using namespace std;

const vector<string> COLORS{"black", "brown", "red",    "orange", "yellow",
                            "green", "blue",  "violet", "grey",   "white"};

inline int color_code(string name) {
    for (size_t i = 0; i < COLORS.size(); i++) {
        if (COLORS[i] == name) {
            return i;
        }
    }
    throw domain_error{""};
}

inline vector<string> colors() { return COLORS; }

}  // namespace resistor_color
