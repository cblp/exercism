#include "pascals_triangle.h"

#include <assert.h>
#include <stdlib.h>

uint8_t** create_triangle(size_t rows) {
    if (rows == 0) {
        uint8_t** triangle = malloc(sizeof(uint8_t*));
        assert(triangle);
        triangle[0] = malloc(sizeof(uint8_t));
        assert(triangle[0]);
        triangle[0][0] = 0;
        return triangle;
    }

    uint8_t** triangle = malloc(rows * sizeof(uint8_t*));
    assert(triangle);

    for (size_t i = 0; i < rows; i++) {
        triangle[i] = calloc(rows, sizeof(uint8_t));
        assert(triangle[i]);

        triangle[i][0] = 1;
        triangle[i][i] = 1;

        for (size_t j = 1; j < i; j++) {
            triangle[i][j] = triangle[i - 1][j - 1] + triangle[i - 1][j];
        }
    }

    return triangle;
}

void free_triangle(uint8_t** triangle, size_t rows) {
    if (!triangle) {
        return;
    }
    if (rows == 0) {
        rows = 1;
    }
    for (size_t i = 0; i < rows; i++) {
        free(triangle[i]);
    }
    free(triangle);
}
