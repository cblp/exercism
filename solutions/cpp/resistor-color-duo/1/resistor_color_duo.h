#pragma once

#include <stdexcept>
#include <string>
#include <vector>

namespace resistor_color_duo {

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

inline int value(vector<string> names) {
    return color_code(names[0]) * 10 + color_code(names[1]);
}

}  // namespace resistor_color_duo
