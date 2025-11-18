#include "leap.h"

bool divisible_by(int a, int b)
{
    return a % b == 0;
}

bool leap_year(int year)
{
    return divisible_by(year, 4) && !(divisible_by(year, 100) && !divisible_by(year, 400));
}
