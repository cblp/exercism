#include "variable_length_quantity.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

/// @brief base-128 representation in inverted order, from least to most
/// significant digits
///
/// @param[in] n input number
/// @param[out] digits output array
/// @return number of used digits
size_t to_base_128(uint32_t n, uint8_t digits[5]) {
    if (n == 0) {
        digits[0] = 0;
        return 1;
    }
    size_t digits_count = 0;
    while (n != 0) {
        uint8_t digit = n % 128;
        n /= 128;
        digits[digits_count++] = digit;
    }
    return digits_count;
}

int encode(const uint32_t* integers, size_t integers_len, uint8_t* output) {
    assert(integers);
    assert(output);

    int output_len = 0;
    for (size_t i = 0; i < integers_len; ++i) {
        uint8_t digits[5];
        size_t const digits_count = to_base_128(integers[i], digits);
        for (int j = digits_count - 1; j >= 0; --j) {
            output[output_len++] = (j ? 0x80 : 0) | digits[j];
        }
    }
    return output_len;
}

int decode(const uint8_t* bytes, size_t buffer_len, uint32_t* output) {
    assert(bytes);
    assert(output);

    uint32_t n = 0;
    bool in_sequence = false;
    int output_len = 0;
    for (size_t i = 0; i < buffer_len; ++i) {
        n = n * 128 + (bytes[i] & 0x7F);
        in_sequence = bytes[i] & 0x80;
        if (!in_sequence) {
            output[output_len++] = n;
            n = 0;
        }
    }
    if (in_sequence) {
        return -1;
    }
    return output_len;
}
