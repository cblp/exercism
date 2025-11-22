#include "grade_school.h"

#include <string.h>

void init_roster(roster_t* roster) { roster->count = 0; }

bool add_student(roster_t* roster, const char* name, uint8_t grade) {
    if (roster->count >= MAX_STUDENTS) {
        return false;
    }
    for (size_t i = 0; i < roster->count; ++i) {
        if (strcmp(roster->students[i].name, name) == 0) {
            return false;
        }
    }
    for (size_t i = 0; i < roster->count; ++i) {
        if (roster->students[i].grade > grade ||
            (roster->students[i].grade == grade &&
             strcmp(roster->students[i].name, name) > 0)) {
            for (size_t j = roster->count; j > i; --j) {
                roster->students[j] = roster->students[j - 1];
            }
            roster->students[i].grade = grade;
            strcpy(roster->students[i].name, name);
            roster->count++;
            return true;
        }
    }
    student_t* student = &roster->students[roster->count++];
    student->grade = grade;
    strcpy(student->name, name);
    return true;
}

roster_t get_grade(const roster_t* roster, uint8_t grade) {
    roster_t result;
    init_roster(&result);
    for (size_t i = 0; i < roster->count; ++i) {
        if (roster->students[i].grade == grade) {
            add_student(&result, roster->students[i].name, grade);
        }
    }
    return result;
}
