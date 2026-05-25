#pragma once

#include <string>
#include <vector>

namespace flower_field {

using namespace std;

inline vector<string> annotate(vector<string> field) {
    for (size_t i = 0; i < field.size(); i++) {
        for (size_t j = 0; j < field[i].size(); j++) {
            if (field[i][j] == '*') {
                continue;
            }
            int count = 0;
            for (int x = -1; x <= 1; x++) {
                for (int y = -1; y <= 1; y++) {
                    if (i + x >= 0
                        && i + x < field.size()
                        && j + y >= 0
                        && j + y < field[i].size()
                        && field[i + x][j + y] == '*') {
                        count++;
                    }
                }
            }
            field[i][j] = count > 0 ? '0' + count : ' ';
        }
    }
    return field;
}

}  // namespace flower_field
