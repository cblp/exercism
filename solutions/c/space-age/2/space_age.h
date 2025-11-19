#ifndef SPACE_AGE_H
#define SPACE_AGE_H

#include <stdbool.h>
#include <stdint.h>

typedef enum planet {
    MERCURY,
    VENUS,
    EARTH,
    MARS,
    JUPITER,
    SATURN,
    URANUS,
    NEPTUNE,
    _PLANET_COUNT,
} planet_t;

bool planet_is_valid(planet_t);

float age(planet_t planet, int64_t seconds);

#endif
