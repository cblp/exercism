#include <stdint.h>

#include "resistor_color_trio.h"

#define KILO 1000
#define MEGA (1000 * 1000)
#define GIGA (1000 * 1000 * 1000)

uintmax_t POW10[] = {
    1,
    10,
    100,
    1000,
    10 * 1000,
    100 * 1000,
    1000 * 1000,
    10 * 1000 * 1000,
    100 * 1000 * 1000,
    1000 * 1000 * 1000,
};

resistor_value_t
color_code(resistor_band_t bands[])
{
    uintmax_t value = (bands[0] * 10 + bands[1]) * POW10[bands[2]];
    resistor_unit_t unit = OHMS;
    if (value > GIGA && value % GIGA == 0)
    {
        value /= GIGA;
        unit = GIGAOHMS;
    }
    if (value > MEGA && value % MEGA == 0)
    {
        value /= MEGA;
        unit = MEGAOHMS;
    }
    if (value > KILO && value % KILO == 0)
    {
        value /= KILO;
        unit = KILOOHMS;
    }
    return (resistor_value_t){.unit = unit, .value = value};
}
