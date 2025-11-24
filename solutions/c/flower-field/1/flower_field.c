#include "flower_field.h"

#include <stdlib.h>
#include <string.h>

static void annotate_cell(const char** const garden, char** const annotation,
                          int const rows, int const i, int const j) {
    if (garden[i][j] == '*') {
        return;
    }

    int flower_count = 0;
    for (int di = -1; di <= 1; di++) {
        int const ni = i + di;
        if (ni < 0 || ni >= rows) continue;
        for (int dj = -1; dj <= 1; dj++) {
            if (di == 0 && dj == 0) continue;
            int nj = j + dj;
            if (nj < 0 || garden[i][nj] == 0) continue;
            if (garden[ni][nj] == '*') {
                flower_count++;
            }
        }
    }

    if (flower_count > 0) {
        annotation[i][j] = '0' + flower_count;
    }
}

char** annotate(const char** garden, size_t rows) {
    if (rows == 0 || garden == NULL) return NULL;
    // size_t cols = strlen(garden[0]);

    char** annotation = calloc(rows + 1, sizeof(char*));
    for (size_t i = 0; i < rows; i++) {
        if (garden[i] == NULL) {
            annotation[i] = NULL;
            continue;
        }
        annotation[i] = strdup(garden[i]);
        for (size_t j = 0; garden[i][j]; j++)
            annotate_cell(garden, annotation, rows, i, j);
    }
    return annotation;
}

void free_annotation(char** annotation) {
    if (annotation == NULL) return;
    for (size_t i = 0; annotation[i]; i++) free(annotation[i]);
    free(annotation);
}
