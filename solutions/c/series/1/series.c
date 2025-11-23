#include "series.h"

#include <stdlib.h>
#include <string.h>

slices_t slices(char* input_text, unsigned substring_length) {
    slices_t result = {0};

    // Validate input parameters
    if (substring_length == 0 || substring_length > MAX_SERIES_LENGTH) {
        // return empty result for invalid substring length
        return result;
    }

    unsigned input_length = 0;
    while (input_text[input_length] && input_length < MAX_INPUT_TEXT_LENGTH) {
        input_length++;
    }

    if (substring_length > input_length) {
        // return empty result if substring length exceeds input length
        return result;
    }

    // Calculate number of substrings
    result.substring_count = input_length - substring_length + 1;
    result.substring = malloc(result.substring_count * sizeof(char*));

    for (unsigned i = 0; i < result.substring_count; i++) {
        result.substring[i] = malloc(substring_length + 1);
        strncpy(result.substring[i], &input_text[i], substring_length);
        result.substring[i][substring_length] = 0;
    }

    return result;
}
