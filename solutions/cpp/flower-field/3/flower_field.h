#pragma once

#include <string>
#include <vector>

namespace flower_field {

using namespace std;

inline vector<string> annotate(vector<string> field) {
    for (int i = 0; i < int(field.size()); i++) {
        for (int j = 0; j < int(field[i].size()); j++) {
            if (field[i][j] == '*') {
                continue;
            }
            int count = 0;
            for (int x = -1; x <= 1; x++) {
                for (int y = -1; y <= 1; y++) {
                    count += i + x >= 0
                         and i + x < int(field.size())
                         and j + y >= 0
                         and j + y < int(field[i].size())
                         and field[i + x][j + y] == '*';
                }
            }
            field[i][j] = count > 0 ? '0' + count : ' ';
        }
    }
    return field;
}

}  // namespace flower_field
