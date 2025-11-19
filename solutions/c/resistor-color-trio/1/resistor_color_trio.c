#include <stdint.h>

#include "resistor_color_trio.h"

#define KILO 1000
#define MEGA (KILO * 1000)
#define GIGA (MEGA * 1000)

/// @brief Computes the value of `base` raised to the power `exponent`.
uintmax_t pow_unsigned(uintmax_t base, uintmax_t exponent)
{
    uintmax_t r = 1;
    for (uintmax_t i = 0; i < exponent; ++i)
        r *= base;
    return r;
}

resistor_value_t color_code(resistor_band_t bands[])
{
    uintmax_t value = (bands[0] * 10 + bands[1]) * pow_unsigned(10, bands[2]);
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
