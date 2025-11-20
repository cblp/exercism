#include "high_scores.h"

int32_t latest(const int32_t* scores, size_t scores_len) {
    return scores[scores_len - 1];
}

int32_t personal_best(const int32_t* scores, size_t scores_len) {
    int32_t r = scores[0];
    for (size_t i = 1; i < scores_len; ++i) {
        if (scores[i] > r) r = scores[i];
    }
    return r;
}

// insert `x` in array `out:out_len` at `i`,
// keeping it sorted (highest to lowest)
void insert(int32_t x, int32_t* out, size_t out_len, size_t i);
void insert(int32_t x, int32_t* out, size_t out_len, size_t i) {
    if (i >= 3) return;
    if (i >= out_len) {
        out[i] = x;
    } else if (x > out[i]) {
        int32_t const y = out[i];
        out[i] = x;
        insert(y, out, out_len, i + 1);
    } else {
        insert(x, out, out_len, i + 1);
    }
}

size_t personal_top_three(const int32_t* scores, size_t scores_len,
                          int32_t* out) {
    size_t out_len = 0;
    for (size_t i = 0; i < scores_len; ++i) {
        insert(scores[i], out, out_len, 0);
        if (out_len < 3) ++out_len;
    }
    return out_len;
}
