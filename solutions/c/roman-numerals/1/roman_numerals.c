#include "roman_numerals.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

struct RomanDigit {
    unsigned int value;
    const char* symbol;
} roman_digits[] = {
    {1000, "M"}, {900, "CM"}, {500, "D"}, {400, "CD"},  //
    {100, "C"},  {90, "XC"},  {50, "L"},  {40, "XL"},   //
    {10, "X"},   {9, "IX"},   {5, "V"},   {4, "IV"},    //
    {1, "I"},
};

char* to_roman_numeral(unsigned int number) {
    if (number == 0 || number > 3999) {
        return NULL;
    }
    char* result = malloc(20);
    assert(result);
    result[0] = 0;
    for (size_t i = 0; i < 13; ++i) {
        while (number >= roman_digits[i].value) {
            strcat(result, roman_digits[i].symbol);
            number -= roman_digits[i].value;
        }
    }
    return result;
}
