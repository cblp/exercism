#pragma once

#include <algorithm>
#include <array>
#include <map>
#include <string>
#include <vector>

namespace kindergarten_garden {

using namespace std;

enum Plants { clover, grass, violets, radishes };

inline Plants plant_by_char(char c) {
    return map<char, Plants>{
        {'C', clover}, {'G', grass}, {'R', radishes}, {'V', violets}}[c];
}

const vector<string> STUDENTS{"Alice",  "Bob",    "Charlie", "David",
                              "Eve",    "Fred",   "Ginny",   "Harriet",
                              "Ileana", "Joseph", "Kincaid", "Larry"};

inline array<Plants, 4> plants(string lines, string student) {
    const auto second_line_offset = lines.size() / 2 + 1;
    const auto i =
        find(STUDENTS.begin(), STUDENTS.end(), student) - STUDENTS.begin();
    return {plant_by_char(lines[i * 2]), plant_by_char(lines[i * 2 + 1]),
            plant_by_char(lines[second_line_offset + i * 2]),
            plant_by_char(lines[second_line_offset + i * 2 + 1])};
}

}  // namespace kindergarten_garden
