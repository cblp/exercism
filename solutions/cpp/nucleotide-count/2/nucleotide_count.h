#pragma once

#include <map>
#include <stdexcept>
#include <string>

namespace nucleotide_count {

using namespace std;

inline map<char, int> count(const string dna) {
    int a{}, c{}, g{}, t{};
    for (const char n : dna) {
        switch (n) {
            case 'A':
                ++a;
                break;
            case 'C':
                ++c;
                break;
            case 'G':
                ++g;
                break;
            case 'T':
                ++t;
                break;
            default:
                throw invalid_argument{string{n}};
        }
    }
    return {{'A', a}, {'C', c}, {'G', g}, {'T', t}};
}

}  // namespace nucleotide_count
