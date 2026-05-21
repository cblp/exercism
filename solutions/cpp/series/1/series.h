#pragma once

#include <string>
#include <vector>

namespace series {

using namespace std;

inline vector<string> slice(string source, ssize_t n) {
    if (n < 1 or size_t(n) > source.size()) {
        throw domain_error{""};
    }
    vector<string> slices;
    for (size_t i = 0; i <= source.size() - n; i++) {
        slices.push_back(source.substr(i, n));
    }
    return slices;
}

}  // namespace series
