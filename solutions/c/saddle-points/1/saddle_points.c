#include "saddle_points.h"

#include <assert.h>
#include <stdlib.h>

saddle_points_t* saddle_points(size_t rows, size_t columns,
                               uint8_t matrix[rows][columns]) {
    saddle_points_t* result = calloc(1, sizeof(saddle_points_t));
    assert(result);

    for (size_t r = 0; r < rows; r++) {
        size_t row_max_indexes[MAX_SADDLE_POINTS] = {0};
        size_t row_max_indexes_count = 1;
        for (size_t c = 1; c < columns; c++) {
            uint8_t a = matrix[r][row_max_indexes[0]];
            uint8_t b = matrix[r][c];
            if (a == b) {
                assert(row_max_indexes_count < MAX_SADDLE_POINTS);
                row_max_indexes[row_max_indexes_count++] = c;
            } else if (a < b) {
                row_max_indexes[0] = c;
                row_max_indexes_count = 1;
            }
        }
        for (size_t j = 0; j < row_max_indexes_count; j++) {
            size_t c = row_max_indexes[j];
            // Check if it's also the minimum in its column
            uint8_t candidate = matrix[r][c];
            for (size_t i = 0; i < rows; i++) {
                if (matrix[i][c] < candidate) {
                    goto not_min_in_column;
                }
            }
            assert(result->count < MAX_SADDLE_POINTS);
            result->points[result->count++] = (saddle_point_t){r + 1, c + 1};
        not_min_in_column:;
        }
    }
    return result;
}

void free_saddle_points(saddle_points_t* saddle_points) { free(saddle_points); }
