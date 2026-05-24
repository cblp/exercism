#pragma once

#include <string>

namespace rotational_cipher {

using namespace std;

inline char rotate_letter(char c, char base, int key) {
    return (c - base + key) % 26 + base;
}

inline char rotate_char(char c, int key) {
    if (isupper(c)) {
        return rotate_letter(c, 'A', key);
    }
    if (islower(c)) {
        return rotate_letter(c, 'a', key);
    }
    return c;
}

inline string rotate(string text, int key) {
    for (char& c : text) {
        c = rotate_char(c, key);
    }
    return text;
}

}  // namespace rotational_cipher
