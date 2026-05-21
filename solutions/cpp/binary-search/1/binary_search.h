#pragma once

#include <stdexcept>
#include <vector>

namespace binary_search {

using namespace std;

inline size_t find(vector<int> data, int value) {
    int lo = 0, hi = int(data.size()) - 1;
    while (lo <= hi) {
        const size_t mid = lo + (hi - lo) / 2;
        if (value == data[mid]) {
            return mid;
        } else if (value < data[mid]) {
            hi = mid - 1;
        } else {
            lo = mid + 1;
        }
    }
    throw domain_error{"not found"};
}

}  // namespace binary_search
