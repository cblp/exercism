#include "spiral_matrix.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

spiral_matrix_t* spiral_matrix_allocate(size_t size);
spiral_matrix_t* spiral_matrix_allocate(size_t size) {
    spiral_matrix_t* matrix = calloc(1, sizeof(spiral_matrix_t));
    matrix->size = size;
    if (size == 0) {
        return matrix;
    }
    matrix->matrix = calloc(size, sizeof(int*));
    int* data = calloc(size * size, sizeof(int));
    for (size_t i = 0; i < size; i++) {
        matrix->matrix[i] = data + i * size;
    }
    return matrix;
}

typedef struct {
    size_t x, y;
} pos_t;

typedef enum {
    RIGHT,
    DOWN,
    LEFT,
    UP,
} direction_t;

pos_t pos_forward(direction_t dir, pos_t p);
pos_t pos_forward(direction_t dir, pos_t p) {
    switch (dir) {
        case RIGHT:
            return (pos_t){p.x + 1, p.y};
        case DOWN:
            return (pos_t){p.x, p.y + 1};
        case LEFT:
            return (pos_t){p.x - 1, p.y};
        case UP:
            return (pos_t){p.x, p.y - 1};
    }
    assert(!"unreachable");
}

bool pos_is_valid(pos_t p, size_t size);
bool pos_is_valid(pos_t p, size_t size) { return p.x < size && p.y < size; }

spiral_matrix_t* spiral_matrix_create(int size) {
    if (size < 0) {
        return NULL;
    }
    spiral_matrix_t* matrix = spiral_matrix_allocate(size);
    if (size == 0) {
        return matrix;
    }
    if (size == 1) {
        matrix->matrix[0][0] = 1;
        return matrix;
    }
    pos_t p = {0, 0};
    direction_t dir = RIGHT;
    for (int x = 1; x <= size * size; x++) {
        matrix->matrix[p.y][p.x] = x;
        pos_t const next = pos_forward(dir, p);
        if (pos_is_valid(next, size) && matrix->matrix[next.y][next.x] == 0) {
            p = next;
        } else {
            dir = (direction_t)((dir + 1) % 4);
            p = pos_forward(dir, p);
        }
    }
    return matrix;
}

void spiral_matrix_destroy(spiral_matrix_t* matrix) {
    if (matrix == NULL) {
        return;
    }
    if (matrix->matrix != NULL) {
        free(matrix->matrix[0]);
        free(matrix->matrix);
    }
    free(matrix);
}
