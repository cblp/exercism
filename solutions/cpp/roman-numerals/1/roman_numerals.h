#pragma once

#include <string>
#include <tuple>
#include <vector>

namespace roman_numerals {

using namespace std;

const vector<pair<unsigned, string>> ROMAN_DIGITS{
    {1000, "M"}, {900, "CM"}, {500, "D"}, {400, "CD"}, {100, "C"},
    {90, "XC"},  {50, "L"},   {40, "XL"}, {10, "X"},   {9, "IX"},
    {5, "V"},    {4, "IV"},   {1, "I"}};

inline string convert(unsigned n) {
    string r;
    for (const auto& [value, symbol] : ROMAN_DIGITS) {
        while (n >= value) {
            r += symbol;
            n -= value;
        }
    }
    return r;
}

}  // namespace roman_numerals
