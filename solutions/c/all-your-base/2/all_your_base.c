#include "all_your_base.h"

size_t rebase(int8_t digits[DIGITS_ARRAY_SIZE], int16_t input_base,
              int16_t output_base, size_t input_length) {
    if (input_length < 1 || input_base < 2 || output_base < 2) {
        return 0;
    }

    uintmax_t n = 0;
    for (size_t i = 0; i < input_length; ++i) {
        if (digits[i] < 0 || digits[i] >= input_base) {
            return 0;
        }
        n = n * input_base + digits[i];
    }

    if (n == 0) {
        digits[0] = 0;
        return 1;
    }

    size_t output_length = 0;
    while (n) {
        digits[output_length++] = n % output_base;
        n /= output_base;
    }
    // reverse inplace
    for (size_t i = 0; i < output_length / 2; ++i) {
        int8_t const tmp = digits[i];
        digits[i] = digits[output_length - 1 - i];
        digits[output_length - 1 - i] = tmp;
    }
    return output_length;
}
