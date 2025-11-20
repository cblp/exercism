#include "allergies.h"

bool is_allergic_to(allergen_t allergen, uint16_t flags) {
    return (1 << allergen) & flags;
}

allergen_list_t get_allergens(uint16_t flags) {
    allergen_list_t list = {0};
    for (allergen_t allergen = 0; allergen < ALLERGEN_COUNT; ++allergen) {
        if (is_allergic_to(allergen, flags)) {
            list.count++;
            list.allergens[allergen] = true;
        }
    }
    return list;
}
