#include "circular_buffer.h"

#include <assert.h>
#include <errno.h>

void _advance_reader(circular_buffer_t* buffer);
void _advance_reader(circular_buffer_t* buffer) {
    assert(buffer);
    buffer->reader = (buffer->reader + 1) % buffer->phys_capacity;
}

void _write_and_advance_writer(circular_buffer_t*, buffer_value_t);
void _write_and_advance_writer(circular_buffer_t* buffer,
                               buffer_value_t const value) {
    assert(buffer);
    buffer->data[buffer->writer] = value;
    buffer->writer = (buffer->writer + 1) % buffer->phys_capacity;
}

circular_buffer_t* new_circular_buffer(size_t requested_capacity) {
    assert(requested_capacity);
    size_t phys_capacity = requested_capacity + 1;
    buffer_value_t* data = calloc(phys_capacity, sizeof(buffer_value_t));
    assert(data);
    circular_buffer_t* buffer = calloc(1, sizeof(circular_buffer_t));
    assert(buffer);
    buffer->data = data;
    buffer->phys_capacity = phys_capacity;
    return buffer;
}

void delete_buffer(circular_buffer_t* buffer) { assert(buffer); }

void clear_buffer(circular_buffer_t* buffer) {
    assert(buffer);
    buffer->reader = 0;
    buffer->writer = 0;
}

int write(circular_buffer_t* buffer, buffer_value_t value) {
    assert(buffer);
    if (is_full(buffer)) {
        errno = ENOBUFS;
        return EXIT_FAILURE;
    }
    _write_and_advance_writer(buffer, value);
    return EXIT_SUCCESS;
}

int overwrite(circular_buffer_t* buffer, buffer_value_t value) {
    assert(buffer);
    if (is_full(buffer)) {
        _advance_reader(buffer);
    }
    _write_and_advance_writer(buffer, value);
    return EXIT_SUCCESS;
}

int read(circular_buffer_t* buffer, buffer_value_t* value) {
    assert(buffer);
    if (is_empty(buffer)) {
        errno = ENODATA;
        return EXIT_FAILURE;
    }
    *value = buffer->data[buffer->reader];
    _advance_reader(buffer);
    return EXIT_SUCCESS;
}

bool is_full(circular_buffer_t* buffer) {
    assert(buffer);
    return buffer->reader == (buffer->writer + 1) % buffer->phys_capacity;
}

bool is_empty(circular_buffer_t* buffer) {
    assert(buffer);
    return buffer->reader == buffer->writer;
}
