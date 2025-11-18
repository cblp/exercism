#include "resistor_color.h"

resistor_band_t COLORS[] = {
    BLACK,
    BROWN,
    RED,
    ORANGE,
    YELLOW,
    GREEN,
    BLUE,
    VIOLET,
    GREY,
    WHITE,
};

int color_code(resistor_band_t b)
{
    return b;
}

resistor_band_t *colors(void)
{
    return COLORS;
}
