#include "zebra_puzzle.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

typedef enum { Color, Drink, Hobby, Nation, Pet } Feature;

enum { Blue = Color * 10, Green, Ivory, Red, Yellow };
const char* const COLOR_NAMES[] = {"Blue", "Green", "Ivory", "Red", "Yellow"};

enum { Coffee = Drink * 10, Juice, Milk, Tea, Water };
const char* const DRINK_NAMES[] = {"Coffee", "Juice", "Milk", "Tea", "Water"};

enum { Chess = Hobby * 10, Dance, Football, Paint, Read };
const char* const HOBBY_NAMES[] = {"Chess", "Dance", "Football", "Paint",
                                   "Read"};

enum { Englishman = Nation * 10, Japanese, Norwegian, Spaniard, Ukrainian };
const char* const NATION_NAMES[] = {"Englishman", "Japanese", "Norwegian",
                                    "Spaniard", "Ukrainian"};

enum { Dog = Pet * 10, Fox, Horse, Snail, Zebra };
const char* const PET_NAMES[] = {"Dog", "Fox", "Horse", "Snail", "Zebra"};

static void swap(int* arr, size_t i, size_t j) {
    int tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
}

int features[5][5];

static bool next_permutation(Feature const f) {
    int* const tuple = features[f];

    if (tuple[0] == tuple[1])  // start
    {
        for (size_t k = 0; k < 5; k++) {
            tuple[k] = k + f * 10;
        }
        return true;
    }

    int i = 3;
    while (i >= 0 && tuple[i] >= tuple[i + 1]) i--;
    if (i < 0) return false;

    int j = 4;
    while (tuple[j] <= tuple[i]) j--;

    swap(tuple, i, j);

    // reverse tuple[i+1 .. 4]
    for (int left = i + 1, right = 4; left < right; left++, right--)
        swap(tuple, left, right);

    return true;
}

static bool left_right(int x, int y) {
    const int* const xs = features[x / 10];
    const int* const ys = features[y / 10];
    return (xs[0] == x && ys[1] == y) || (xs[1] == x && ys[2] == y) ||
           (xs[2] == x && ys[3] == y) || (xs[3] == x && ys[4] == y);
}

static bool next_to(int x, int y) {
    return left_right(x, y) || left_right(y, x);
}

static bool same(int x, int y) {
    const int* const xs = features[x / 10];
    const int* const ys = features[y / 10];
    return (xs[0] == x && ys[0] == y) || (xs[1] == x && ys[1] == y) ||
           (xs[2] == x && ys[2] == y) || (xs[3] == x && ys[3] == y) ||
           (xs[4] == x && ys[4] == y);
}

#define GUARD(condition) \
    if (!(condition)) continue;

static void reset(Feature f) { memset(features[f], 0, 5 * sizeof(int)); }

static bool check_solution(solution_t* const solution) {
    for (size_t i = 0; i < 5; i++) {
        if (features[Drink][i] == Water)
            solution->drinks_water = NATION_NAMES[features[Nation][i] % 10];
        if (features[Pet][i] == Zebra)
            solution->owns_zebra = NATION_NAMES[features[Nation][i] % 10];
        if (solution->drinks_water && solution->owns_zebra) return true;
    }
    return false;
}

solution_t solve_puzzle(void) {
    solution_t solution = {0};

    reset(Color);
    while (next_permutation(Color)) {
        GUARD(left_right(Ivory, Green));  // 6

        reset(Nation);
        while (next_permutation(Nation)) {
            GUARD(features[Nation][0] == Norwegian);  // 10
            GUARD(same(Englishman, Red));             // 2
            GUARD(next_to(Norwegian, Blue));          // 15

            reset(Drink);
            while (next_permutation(Drink)) {
                GUARD(features[Drink][2] == Milk);  // 9
                GUARD(same(Green, Coffee));         // 4
                GUARD(same(Ukrainian, Tea));        // 5

                reset(Pet);
                while (next_permutation(Pet)) {
                    GUARD(same(Spaniard, Dog));  // 3

                    reset(Hobby);
                    while (next_permutation(Hobby)) {
                        GUARD(same(Snail, Dance));     // 7
                        GUARD(same(Yellow, Paint));    // 8
                        GUARD(next_to(Read, Fox));     // 11
                        GUARD(next_to(Paint, Horse));  // 12
                        GUARD(same(Football, Juice));  // 13
                        GUARD(same(Japanese, Chess));  // 14

                        if (check_solution(&solution)) return solution;
                    }
                }
            }
        }
    }

    assert(!"not found");
}
