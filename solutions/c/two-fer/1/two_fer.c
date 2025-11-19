#include "two_fer.h"

#include <stdio.h>

void two_fer(char* buffer, const char* name) {
    snprintf(buffer, 100 /* BUFFER_SIZE from test */, "One for %s, one for me.",
             name ? name : "you");
}
