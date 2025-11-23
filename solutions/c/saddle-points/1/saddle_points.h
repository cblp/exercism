#ifndef SADDLE_POINTS_H
#define SADDLE_POINTS_H

#include <stddef.h>
#include <stdint.h>

typedef struct {
    int row;
    int column;
} saddle_point_t;

#define MAX_SADDLE_POINTS 100

typedef struct {
    size_t count;
    saddle_point_t points[MAX_SADDLE_POINTS];
} saddle_points_t;

saddle_points_t* saddle_points(size_t rows, size_t columns,
                               uint8_t matrix[rows][columns]);

void free_saddle_points(saddle_points_t* saddle_points);

#endif
