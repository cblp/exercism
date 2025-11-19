#include "darts.h"

uint8_t score(coordinate_t pos) {
    float d2 = pos.x * pos.x + pos.y * pos.y;
    return d2 <= 1 ? 10 : d2 <= 25 ? 5 : d2 <= 100 ? 1 : 0;
}
