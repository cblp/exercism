#ifndef RAIL_FENCE_CIPHER_H
#define RAIL_FENCE_CIPHER_H

#include <stddef.h>

char* encode(const char* text, size_t rails);

char* decode(const char* ciphertext, size_t rails);

#endif
