#pragma once

#include <cstdint>
#include <string>
#include <unordered_set>
#include <vector>

namespace allergies {

using namespace std;

const vector<string> KNOWN_ALLERGIES{"eggs",         "peanuts",  "shellfish",
                                     "strawberries", "tomatoes", "chocolate",
                                     "pollen",       "cats"};

struct allergy_test {
    uint16_t bits;

    allergy_test(uint16_t bits) : bits(bits) {}

    bool is_allergic_to(string allergy_name) const {
        for (size_t i = 0; i < KNOWN_ALLERGIES.size(); i++) {
            if (bits & (1 << i) and KNOWN_ALLERGIES[i] == allergy_name)
                return true;
        }
        return false;
    }

    unordered_set<string> get_allergies() const {
        unordered_set<string> allergies;
        for (size_t i = 0; i < KNOWN_ALLERGIES.size(); i++) {
            if (bits & (1 << i)) {
                allergies.insert(KNOWN_ALLERGIES[i]);
            }
        }
        return allergies;
    }
};

}  // namespace allergies
