#include "largest_series_product.h"

#include <ctype.h>
#include <string.h>

int64_t largest_series_product(char* digits, size_t span) {
    size_t digits_len = strlen(digits);
    if (span > digits_len) return -1;
    int64_t max_product = 0;
    for (size_t i = 0; i <= digits_len - span; i++) {
        int64_t product = 1;
        for (size_t j = i; j < i + span; j++) {
            if (isdigit(digits[j]))
                product *= digits[j] - '0';
            else
                return -1;
        }
        if (product > max_product) max_product = product;
    }
    return max_product;
}
