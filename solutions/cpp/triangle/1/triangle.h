#pragma once

#include <stdexcept>

namespace triangle {

enum flavor { equilateral, isosceles, scalene };

inline flavor kind(double a, double b, double c) {
    if (a <= 0 or b <= 0 or c <= 0 or a >= b + c or b >= a + c or c >= a + b) {
        throw std::domain_error{"bad triangle"};
    }
    return a == b and b == c          ? equilateral
         : a == b or b == c or c == a ? isosceles
                                      : scalene;
}

}  // namespace triangle
