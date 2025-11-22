#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H

#include <stdbool.h>
#include <stdlib.h>

typedef int buffer_value_t;

typedef struct {
    buffer_value_t* data;
    size_t phys_capacity;  /// Physical capacity = logical capacity + 1
    size_t reader, writer;
} circular_buffer_t;

circular_buffer_t* new_circular_buffer(size_t capacity);
void delete_buffer(circular_buffer_t* buffer);
void clear_buffer(circular_buffer_t* buffer);
int write(circular_buffer_t* buffer, buffer_value_t value);
int overwrite(circular_buffer_t* buffer, buffer_value_t value);
int read(circular_buffer_t* buffer, buffer_value_t* value);

bool is_full(circular_buffer_t* buffer);
bool is_empty(circular_buffer_t* buffer);

#endif
