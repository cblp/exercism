#include "difference_of_squares.h"

unsigned int sum_of_squares(unsigned int number)
{
    unsigned s = 0;
    for (unsigned x = 1; x <= number; ++x)
    {
        s += x * x;
    }
    return s;
}

unsigned int square_of_sum(unsigned int number)
{
    unsigned s = 0;
    for (unsigned x = 1; x <= number; ++x)
    {
        s += x;
    }
    return s * s;
}

unsigned int difference_of_squares(unsigned int number)
{
    return square_of_sum(number) - sum_of_squares(number);
}
