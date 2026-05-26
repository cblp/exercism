#pragma once

#include <vector>

namespace spiral_matrix {

using namespace std;

inline vector<vector<unsigned>> spiral_matrix(unsigned size) {
    vector<vector<unsigned>> m{size, vector<unsigned>(size, 0)};
    int left = 0, right = size - 1, top = 0, bottom = size - 1, x = 1, i;
    while (left <= right) {
        for (i = left; i <= right; i++, x++) m[top][i] = x;
        top++;
        for (i = top; i <= bottom; i++, x++) m[i][right] = x;
        right--;
        for (i = right; i >= left; i--, x++) m[bottom][i] = x;
        bottom--;
        for (i = bottom; i >= top; i--, x++) m[i][left] = x;
        left++;
    }
    return m;
}

}  // namespace spiral_matrix
