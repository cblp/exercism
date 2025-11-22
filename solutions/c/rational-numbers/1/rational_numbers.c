#include "rational_numbers.h"

#include <math.h>

rational_t reduce(rational_t r) {
    int a = r.numerator;
    int b = r.denominator;
    if (b == 0) {
        // Handle division by zero if necessary
        return r;
    }
    // Compute GCD using Euclidean algorithm
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    int gcd = a;
    r.numerator /= gcd;
    r.denominator /= gcd;
    // Ensure the denominator is positive
    if (r.denominator < 0) {
        r.numerator = -r.numerator;
        r.denominator = -r.denominator;
    }
    return r;
}

rational_t add(rational_t a, rational_t b) {
    return reduce(
        (rational_t){a.numerator * b.denominator + b.numerator * a.denominator,
                     a.denominator * b.denominator});
}

rational_t subtract(rational_t a, rational_t b) {
    return reduce(
        (rational_t){a.numerator * b.denominator - b.numerator * a.denominator,
                     a.denominator * b.denominator});
}

rational_t multiply(rational_t a, rational_t b) {
    return reduce(
        (rational_t){a.numerator * b.numerator, a.denominator * b.denominator});
}

rational_t divide(rational_t a, rational_t b) {
    return reduce(
        (rational_t){a.numerator * b.denominator, a.denominator * b.numerator});
}

rational_t absolute(rational_t r) {
    r = reduce(r);
    if (r.numerator < 0) {
        r.numerator = -r.numerator;
    }
    return r;
}

rational_t exp_rational(rational_t a, int b) {
    if (b == 0) {
        return (rational_t){1, 1};
    }
    if (b < 0) {
        return reciprocal(exp_natural(a, -b));
    }
    return exp_natural(a, b);
}

rational_t exp_natural(rational_t a, int b) {
    rational_t result = {1, 1};
    for (int i = 0; i < b; i++) {
        result.numerator *= a.numerator;
        result.denominator *= a.denominator;
    }
    return reduce(result);
}

float exp_real(int a, rational_t b) {
    return pow((float)a, (float)b.numerator / (float)b.denominator);
}

rational_t reciprocal(rational_t r) {
    return reduce((rational_t){r.denominator, r.numerator});
}
