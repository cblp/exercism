#include "yacht.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int counts[6];
} counter_t;

counter_t const LITTLE_STRAIGHT_COUNTER = {{1, 1, 1, 1, 1, 0}};

counter_t const BIG_STRAIGHT_COUNTER = {{0, 1, 1, 1, 1, 1}};

bool counter_eq(counter_t, counter_t);
bool counter_eq(counter_t a, counter_t b) {
    return memcmp(&a, &b, sizeof(counter_t)) == 0;
}

counter_t counter_from_dice(dice_t);
counter_t counter_from_dice(dice_t const dice) {
    counter_t counter = {0};
    for (size_t i = 0; i < 5; ++i) {
        assert(1 <= dice.faces[i]);
        assert(dice.faces[i] <= 6);
        counter.counts[dice.faces[i] - 1] += 1;
    }
    return counter;
}

int counter_get(counter_t counter, int face);
int counter_get(counter_t counter, int face) {
    assert(1 <= face);
    assert(face <= 6);
    return counter.counts[face - 1];
}

int counter_get_face_by_count(counter_t, int target_count);
int counter_get_face_by_count(counter_t counter, int target_count) {
    for (size_t i = 0; i < 6; ++i)
        if (counter.counts[i] == target_count) return i + 1;
    return 0;
}

int counter_get_face_at_least(counter_t, int target_count);
int counter_get_face_at_least(counter_t counter, int target_count) {
    for (size_t i = 0; i < 6; ++i)
        if (counter.counts[i] >= target_count) return i + 1;
    return 0;
}

int sum(const int[5]);
int sum(const int faces[5]) {
    int r = 0;
    for (size_t i = 0; i < 5; ++i) r += faces[i];
    return r;
}

bool counter_is_little_straight(counter_t);
bool counter_is_little_straight(counter_t counter) {
    int const little_straight[] = {1, 1, 1, 1, 1, 0};
    return memcmp(&counter, little_straight, sizeof(counter)) == 0;
}

bool counter_is_big_straight(counter_t);
bool counter_is_big_straight(counter_t counter) {
    int const big_straight[] = {0, 1, 1, 1, 1, 1};
    return memcmp(&counter, big_straight, sizeof(counter)) == 0;
}

int score(dice_t dice, category_t category) {
    counter_t const counter = counter_from_dice(dice);
    switch (category) {
        case ONES:
        case TWOS:
        case THREES:
        case FOURS:
        case FIVES:
        case SIXES: {
            int const value = category + 1;
            return value * counter_get(counter, value);
        }
        case FULL_HOUSE: {
            int const doublet = counter_get_face_by_count(counter, 2);
            int const triplet = counter_get_face_by_count(counter, 3);
            return doublet && triplet ? doublet * 2 + triplet * 3 : 0;
        }
        case FOUR_OF_A_KIND:
            return counter_get_face_at_least(counter, 4) * 4;
        case LITTLE_STRAIGHT:
            return counter_eq(counter, LITTLE_STRAIGHT_COUNTER) ? 30 : 0;
        case BIG_STRAIGHT:
            return counter_eq(counter, BIG_STRAIGHT_COUNTER) ? 30 : 0;
        case CHOICE:
            return sum(dice.faces);
        case YACHT:
            return counter_get_face_by_count(counter, 5) ? 50 : 0;
        default:
            return -1;
    }
}
