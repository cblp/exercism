#ifndef RATIONAL_NUMBERS_H
#define RATIONAL_NUMBERS_H

typedef struct {
    int numerator;
    int denominator;
} rational_t;

rational_t reduce(rational_t);
rational_t add(rational_t, rational_t);
rational_t subtract(rational_t, rational_t);
rational_t multiply(rational_t, rational_t);
rational_t divide(rational_t, rational_t);
rational_t absolute(rational_t);
rational_t exp_rational(rational_t, int);
float exp_real(int, rational_t);

rational_t exp_natural(rational_t, int);
rational_t reciprocal(rational_t);

#endif
