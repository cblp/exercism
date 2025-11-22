#include "kindergarten_garden.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

plant_t plant(char code);
plant_t plant(char code) {
    switch (code) {
        case 'C':
            return CLOVER;
        case 'G':
            return GRASS;
        case 'R':
            return RADISHES;
        case 'V':
            return VIOLETS;
        default:
            printf("code = 0x%02X\n", code);
            assert(!"plant code must be C|G|R|V");
    }
}

const char* STUDENTS[] = {
    "Alice", "Bob",     "Charlie", "David",  "Eve",     "Fred",
    "Ginny", "Harriet", "Ileana",  "Joseph", "Kincaid", "Larry",
};

size_t find_student(const char* student);
size_t find_student(const char* student) {
    assert(student);
    for (size_t i = 0; i < 12; ++i) {
        if (strcmp(STUDENTS[i], student) == 0) {
            return i;
        }
    }
    assert(!"student not found");
}

plants_t plants(const char* diagram, const char* student) {
    assert(diagram);
    assert(student);

    const char* row1 = diagram;
    char* row2 = strchr(diagram, '\n');
    assert(row2);
    ++row2;

    size_t i = find_student(student);
    return (plants_t){{plant(row1[2 * i]), plant(row1[2 * i + 1]),
                       plant(row2[2 * i]), plant(row2[2 * i + 1])}};
}
