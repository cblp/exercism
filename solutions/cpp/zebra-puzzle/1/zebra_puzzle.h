#pragma once

#include <algorithm>
#include <array>
#include <stdexcept>
#include <string>

namespace zebra_puzzle {

using namespace std;

enum Color { Blue, Green, Ivory, Red, Yellow };
enum Drink { Coffee, Juice, Milk, Tea, Water };
enum Hobby { Chess, Dance, Football, Paint, Read };
enum Nation { Englishman, Japanese, Norwegian, Spaniard, Ukrainian };
enum Pet { Dog, Fox, Horse, Snail, Zebra };

inline string nation_to_string(Nation nation) {
    switch (nation) {
        case Englishman:
            return "Englishman";
        case Japanese:
            return "Japanese";
        case Norwegian:
            return "Norwegian";
        case Spaniard:
            return "Spaniard";
        case Ukrainian:
            return "Ukrainian";
    }
    throw logic_error("invalid nation");
}

template <typename T>
array<T, 5> features;

struct Solution {
    string drinksWater;
    string ownsZebra;

    Solution() {
        for (size_t i = 0; i < 5; i++) {
            if (features<Drink>[i] == Water) {
                drinksWater = nation_to_string(features<Nation>[i]);
            }
            if (features<Pet>[i] == Zebra) {
                ownsZebra = nation_to_string(features<Nation>[i]);
            }
        }
    }
};

template <typename A>
void reset() {
    for (size_t i = 0; i < 5; i++) {
        features<A>[i] = A(i);
    }
}

template <typename A>
bool next_permutation() {
    return std::next_permutation(features<A>.begin(), features<A>.end());
}

template <typename A>
bool at(size_t index, A value) {
    return features<A>[index] == value;
}

template <typename A, typename B>
bool same(A a, B b) {
    for (size_t i = 0; i < 5; i++) {
        if (features<A>[i] == a and features<B>[i] == b) {
            return true;
        }
    }
    return false;
}

template <typename A, typename B>
bool left_right(A left, B right) {
    for (size_t i = 0; i < 4; i++) {
        if (features<A>[i] == left and features<B>[i + 1] == right) {
            return true;
        }
    }
    return false;
}

template <typename A, typename B>
bool next_to(A a, B b) {
    return left_right(a, b) or left_right(b, a);
}

#define GUARD(condition) \
    if (not(condition)) continue;

inline Solution solve() {
    reset<Color>();
    do {
        GUARD(left_right(Ivory, Green));  // 6

        reset<Nation>();
        do {
            GUARD(at(0, Norwegian));          // 10
            GUARD(same(Englishman, Red));     // 2
            GUARD(next_to(Norwegian, Blue));  // 15

            reset<Drink>();
            do {
                GUARD(at(2, Milk));           // 9
                GUARD(same(Green, Coffee));   // 4
                GUARD(same(Ukrainian, Tea));  // 5

                reset<Pet>();
                do {
                    GUARD(same(Spaniard, Dog));  // 3

                    reset<Hobby>();
                    do {
                        GUARD(same(Snail, Dance));     // 7
                        GUARD(same(Yellow, Paint));    // 8
                        GUARD(next_to(Read, Fox));     // 11
                        GUARD(next_to(Paint, Horse));  // 12
                        GUARD(same(Football, Juice));  // 13
                        GUARD(same(Japanese, Chess));  // 14

                        return Solution();
                    } while (next_permutation<Hobby>());
                } while (next_permutation<Pet>());
            } while (next_permutation<Drink>());
        } while (next_permutation<Nation>());
    } while (next_permutation<Color>());

    throw logic_error("not found");
}

}  // namespace zebra_puzzle
