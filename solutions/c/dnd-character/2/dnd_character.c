#include "dnd_character.h"

#include <stdlib.h>

int roll_d6(void);
int roll_d6(void) { return 1 + rand() / ((RAND_MAX + 1u) / 6); }

int ability(void) {
    int sum_4d6 = 0;
    int min_d6 = 42;
    for (int i = 0; i < 4; ++i) {
        int d6 = roll_d6();
        sum_4d6 += d6;
        if (d6 < min_d6) min_d6 = d6;
    }
    return sum_4d6 - min_d6;
}

int modifier(int score) { return score / 2 - 5; }

dnd_character_t make_dnd_character(void) {
    int constitution = ability();
    return (dnd_character_t){
        .strength = ability(),
        .dexterity = ability(),
        .constitution = constitution,
        .intelligence = ability(),
        .wisdom = ability(),
        .charisma = ability(),
        .hitpoints = 10 + modifier(constitution),
    };
}
