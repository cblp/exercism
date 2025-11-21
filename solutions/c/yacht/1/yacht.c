#include "yacht.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef const int faces_t[5];

int count_equal_to(faces_t faces, int x);
int count_equal_to(faces_t faces, int x) {
    int r = 0;
    for (size_t i = 0; i < 5; ++i)
        if (faces[i] == x) ++r;
    return r;
}

bool all_equal(faces_t faces);
bool all_equal(faces_t faces) {
    for (size_t i = 1; i < 5; ++i)
        if (faces[i] != faces[0]) return false;
    return true;
}

int sum(faces_t faces);
int sum(faces_t faces) {
    int r = 0;
    for (size_t i = 0; i < 5; ++i) r += faces[i];
    return r;
}

bool is_full_house(faces_t faces);
bool is_full_house(faces_t faces) {
    int const a_value = faces[0];
    int b_value = 0;
    unsigned a_count = 1;
    unsigned b_count = 0;
    for (size_t i = 1; i < 5; ++i) {
        if (faces[i] == a_value) {
            ++a_count;
        } else if (b_count == 0) {
            b_value = faces[i];
            b_count = 1;
        } else if (faces[i] == b_value) {
            ++b_count;
        } else {
            return false;
        }
    }
    return (a_count == 2 && b_count == 3) || (a_count == 3 && b_count == 2);
}

/** Find the value that is duplicated at least four time.
    If no such value, returns 0.
 */
int find_four_of_a_kind(faces_t faces);
int find_four_of_a_kind(faces_t faces) {
    if (faces[0] == faces[1]) {
        int count = 2;
        if (faces[2] == faces[0]) ++count;
        if (faces[3] == faces[0]) ++count;
        if (faces[4] == faces[0]) ++count;
        return count >= 4 ? faces[0] : 0;
    } else {
        return faces[0] == faces[2] && faces[0] == faces[3] &&
                       faces[0] == faces[4]
                   ? faces[0]
               : faces[1] == faces[2] && faces[1] == faces[3] &&
                       faces[1] == faces[4]
                   ? faces[1]
                   : 0;
    }
}

bool is_little_straight(faces_t faces);
bool is_little_straight(faces_t faces) {
    unsigned counters[6] = {0};
    for (size_t i = 0; i < 5; ++i) {
        if (faces[i] < 1 || faces[i] > 6) return false;
        counters[faces[i] - 1] += 1;
    }
    unsigned const little_straight[] = {1, 1, 1, 1, 1, 0};
    return memcmp(counters, little_straight, sizeof(counters)) == 0;
}

bool is_big_straight(faces_t faces);
bool is_big_straight(faces_t faces) {
    unsigned counters[6] = {0};
    for (size_t i = 0; i < 5; ++i) {
        if (faces[i] < 1 || faces[i] > 6) return false;
        counters[faces[i] - 1] += 1;
    }
    unsigned const big_straight[] = {0, 1, 1, 1, 1, 1};
    return memcmp(counters, big_straight, sizeof(counters)) == 0;
}

int score(dice_t dice, category_t category) {
    const int* const faces = dice.faces;
    switch (category) {
        case ONES:
        case TWOS:
        case THREES:
        case FOURS:
        case FIVES:
        case SIXES: {
            int const value = category + 1;
            return value * count_equal_to(faces, value);
        }
        case FULL_HOUSE:
            return is_full_house(faces) ? sum(faces) : 0;
        case FOUR_OF_A_KIND:
            return find_four_of_a_kind(faces) * 4;
        case LITTLE_STRAIGHT:
            return is_little_straight(faces) ? 30 : 0;
        case BIG_STRAIGHT:
            return is_big_straight(faces) ? 30 : 0;
        case CHOICE:
            return sum(faces);
        case YACHT:
            return all_equal(faces) ? 50 : 0;
        default:
            return -1;
    }
}
