#pragma once

#include <stdexcept>
#include <string>

namespace largest_series_product {

using namespace std;

inline int largest_product(string input, size_t size) {
    if (size > input.size()) {
        throw domain_error{""};
    }
    int max_product = 0;
    for (size_t i = 0; i <= input.size() - size; i++) {
        int product = 1;
        for (size_t j = 0; j < size; j++) {
            char c = input[i + j];
            if (not isdigit(c)) {
                throw domain_error{""};
            }
            product *= c - '0';
        }
        if (product > max_product) {
            max_product = product;
        }
    }
    return max_product;
}

}  // namespace largest_series_product
