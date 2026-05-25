#pragma once

#include <cstdlib>

namespace dnd_character {

inline int ability() { return 3 + rand() % 16; }

inline int modifier(int a) { return a / 2 - 5; }

struct Character {
    int charisma, constitution, dexterity, hitpoints, intelligence, strength,
        wisdom;

    Character()
        : charisma(ability()),
          constitution(ability()),
          dexterity(ability()),
          hitpoints(10 + modifier(constitution)),
          intelligence(ability()),
          strength(ability()),
          wisdom(ability()) {}
};

}  // namespace dnd_character
