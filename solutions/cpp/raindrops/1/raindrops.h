#pragma once

#include <string>

namespace raindrops {

using namespace std;

inline string convert(int n) {
    string res;
    if (n % 3 == 0) {
        res += "Pling";
    }
    if (n % 5 == 0) {
        res += "Plang";
    }
    if (n % 7 == 0) {
        res += "Plong";
    }
    if (res.empty()) {
        res = to_string(n);
    }
    return res;
}

}  // namespace raindrops
