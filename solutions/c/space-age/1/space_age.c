#include "space_age.h"

float YEAR_FACTORS[] = {
    0.2408467,   // MERCURY
    0.61519726,  // VENUS
    1,           // EARTH
    1.8808158,   // MARS
    11.862615,   // JUPITER
    29.447498,   // SATURN
    84.016846,   // URANUS
    164.79132,   // NEPTUNE
};

float age(planet_t planet, int64_t seconds) {
    return seconds / (31557600 * YEAR_FACTORS[planet]);
}
