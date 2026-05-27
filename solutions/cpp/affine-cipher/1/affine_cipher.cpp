#include "affine_cipher.h"

#include <stdexcept>

namespace affine_cipher {

using namespace std;

bool is_coprime_with_26(unsigned a) {
    switch (a) {
        case 1:
        case 3:
        case 5:
        case 7:
        case 9:
        case 11:
        case 15:
        case 17:
        case 19:
        case 21:
        case 23:
        case 25:
            return true;
        default:
            return false;
    }
}

string space(unsigned n) { return n != 0 && n % 5 == 0 ? " " : ""; }

string encode(const string& input, unsigned a, unsigned b) {
    if (not is_coprime_with_26(a)) {
        throw invalid_argument("a must be coprime with 26");
    }
    unsigned n = 0;
    string r;
    for (char x : input) {
        if (isalpha(x)) {
            const unsigned i = tolower(x) - 'a';
            const unsigned e = (a * i + b) % 26;
            r += space(n++) + string{char('a' + e)};
        } else if (isdigit(x)) {
            r += space(n++) + x;
        }
    }
    return r;
}

unsigned inv(unsigned a) {
    switch (a) {
        case 1:
            return 1;
        case 3:
            return 9;
        case 5:
            return 21;
        case 7:
            return 15;
        case 9:
            return 3;
        case 11:
            return 19;
        case 15:
            return 7;
        case 17:
            return 23;
        case 19:
            return 11;
        case 21:
            return 5;
        case 23:
            return 17;
        case 25:
            return 25;
        default:
            throw invalid_argument("a must be coprime with 26");
    }
}

string decode(const string& input, unsigned a, unsigned b) {
    a = inv(a);
    b %= 26;
    string r;
    for (char x : input) {
        if (isalpha(x)) {
            const unsigned y = tolower(x) - 'a';
            const unsigned d = (a * (y - b + 26)) % 26;
            r += string{char('a' + d)};
        } else if (isdigit(x)) {
            r += x;
        }
    }
    return r;
}

}  // namespace affine_cipher
