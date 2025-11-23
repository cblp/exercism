#include "secret_handshake.h"

#include <stdlib.h>

const char** commands(size_t number) {
    static const char* all_commands[] = {"wink", "double blink",
                                         "close your eyes", "jump"};

    const char** result = calloc(5, sizeof(const char*));
    size_t count = 0;

    int start, end, step;
    if (number & (1 << 4)) {
        // Reverse order flag is set
        start = 3, end = -1, step = -1;
    } else {
        // Normal order
        start = 0, end = 4, step = 1;
    }

    for (int i = start; i != end; i += step) {
        if (number & (1 << i)) result[count++] = all_commands[i];
    }

    result[count] = NULL;  // Null-terminate the array
    return result;
}
