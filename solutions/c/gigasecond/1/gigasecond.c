#include "gigasecond.h"

#include <time.h>

void gigasecond(time_t input, char* output, size_t size) {
    input += 1000 * 1000 * 1000;
    strftime(output, size, "%Y-%m-%d %H:%M:%S", gmtime(&input));
}
