#pragma once

#include <boost/algorithm/string.hpp>
#include <boost/range/adaptors.hpp>
#include <string>

namespace atbash_cipher {

using namespace boost;
using namespace boost::adaptors;
using namespace std;

inline vector<string> chunks(string input, size_t size) {
    vector<string> chunks;
    for (size_t i = 0; i < input.size(); i += size) {
        chunks.push_back(input.substr(i, size));
    }
    return chunks;
}

inline char mirror(char c) { return isalpha(c) ? 'a' + 'z' - c : c; }

/// this fixes the type to char
inline bool is_alphanumeric(char c) { return isalnum(c); }

inline string encode(string text) {
    return join(
        chunks(copy_range<string>(
                   text | filtered(is_alphanumeric) | transformed([](char c) {
                       return mirror(tolower(c));
                   })),
               5),
        " ");
}

inline string decode(string text) {
    return copy_range<string>(
        text | filtered(is_alphanumeric) | transformed(mirror));
}

}  // namespace atbash_cipher
