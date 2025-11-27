#include "zebra_puzzle.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

typedef enum { Blue, Green, Ivory, Red, Yellow } Color;
const char* const COLOR_NAMES[] = {"Blue", "Green", "Ivory", "Red", "Yellow"};

typedef enum { Coffee, Juice, Milk, Tea, Water } Drink;
const char* const DRINK_NAMES[] = {"Coffee", "Juice", "Milk", "Tea", "Water"};

typedef enum { Chess, Dance, Football, Paint, Read } Hobby;
const char* const HOBBY_NAMES[] = {"Chess", "Dance", "Football", "Paint",
                                   "Read"};

typedef enum { Englishman, Japanese, Norwegian, Spaniard, Ukrainian } Nation;
const char* const NATION_NAMES[] = {"Englishman", "Japanese", "Norwegian",
                                    "Spaniard", "Ukrainian"};

typedef enum { Dog, Fox, Horse, Snail, Zebra } Pet;
const char* const PET_NAMES[] = {"Dog", "Fox", "Horse", "Snail", "Zebra"};

static void swap(int* arr, size_t i, size_t j) {
    int tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
}

static bool next_permutation(int tuple[5]) {
    if (tuple[0] == tuple[1])  // start
    {
        memcpy(tuple, (int[]){0, 1, 2, 3, 4}, 5 * sizeof(int));
        return true;
    }

    int i = 3;
    while (i >= 0 && tuple[i] >= tuple[i + 1]) i--;
    if (i < 0) return false;

    int j = 4;
    while (tuple[j] <= tuple[i]) j--;

    // swap tuple[i] and tuple[j]
    swap(tuple, i, j);

    // reverse tuple[i+1 .. 4]
    for (int left = i + 1, right = 4; left < right; left++, right--)
        swap(tuple, left, right);

    return true;
}

static bool left_right(int xs[5], int x, int ys[5], int y) {
    return (xs[0] == x && ys[1] == y) || (xs[1] == x && ys[2] == y) ||
           (xs[2] == x && ys[3] == y) || (xs[3] == x && ys[4] == y);
}

static bool next_to(int xs[5], int x, int ys[5], int y) {
    return left_right(xs, x, ys, y) || left_right(ys, y, xs, x);
}

static bool same(int xs[5], int x, int ys[5], int y) {
    return (xs[0] == x && ys[0] == y) || (xs[1] == x && ys[1] == y) ||
           (xs[2] == x && ys[2] == y) || (xs[3] == x && ys[3] == y) ||
           (xs[4] == x && ys[4] == y);
}

#define GUARD(condition) \
    if (!(condition)) continue;

solution_t solve_puzzle(void) {
    int colors[5] = {0};
    while (next_permutation(colors)) {
        GUARD(left_right(colors, Ivory, colors, Green));  // 6

        int nations[5] = {0};
        while (next_permutation(nations)) {
            GUARD(nations[0] == Norwegian);                    // 10
            GUARD(same(nations, Englishman, colors, Red));     // 2
            GUARD(next_to(nations, Norwegian, colors, Blue));  // 15

            int drinks[5] = {0};
            while (next_permutation(drinks)) {
                GUARD(drinks[2] == Milk);                      // 9
                GUARD(same(colors, Green, drinks, Coffee));    // 4
                GUARD(same(nations, Ukrainian, drinks, Tea));  // 5

                int pets[5] = {0};
                while (next_permutation(pets)) {
                    GUARD(same(nations, Spaniard, pets, Dog));  // 3

                    int hobbies[5] = {0};
                    while (next_permutation(hobbies)) {
                        GUARD(same(pets, Snail, hobbies, Dance));        // 7
                        GUARD(same(colors, Yellow, hobbies, Paint));     // 8
                        GUARD(next_to(hobbies, Read, pets, Fox));        // 11
                        GUARD(next_to(hobbies, Paint, pets, Horse));     // 12
                        GUARD(same(hobbies, Football, drinks, Juice));   // 13
                        GUARD(same(nations, Japanese, hobbies, Chess));  // 14

                        solution_t solution = {0};
                        for (size_t i = 0; i < 5; i++) {
                            if (drinks[i] == Water)
                                solution.drinks_water =
                                    NATION_NAMES[nations[i]];
                            if (pets[i] == Zebra)
                                solution.owns_zebra = NATION_NAMES[nations[i]];
                        }
                        return solution;
                    }
                }
            }
        }
    }

    assert(!"not found");
}
