#include "triangle.h"

bool is_valid(triangle_t sides);
bool is_valid(triangle_t sides) {
    return sides.a > 0 && sides.b > 0 && sides.c > 0 &&
           sides.a < sides.b + sides.c && sides.b < sides.c + sides.a &&
           sides.c < sides.a + sides.b;
}

bool is_equilateral(triangle_t sides) {
    return is_valid(sides) && sides.a == sides.b && sides.b == sides.c;
}

bool is_scalene(triangle_t sides) {
    return is_valid(sides) && !is_isosceles(sides);
}

bool is_isosceles(triangle_t sides) {
    return is_valid(sides) &&
           (sides.a == sides.b || sides.b == sides.c || sides.c == sides.a);
}
