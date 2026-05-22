#pragma once

#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/lambda2.hpp>
#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <map>
#include <set>
#include <string>

namespace anagram {

using namespace boost;
using namespace boost::algorithm;
using namespace boost::adaptors;
using namespace boost::lambda2;
using namespace std;

inline unsigned char char_to_lower(unsigned char c) { return tolower(c); }

inline string string_to_lower(string s) {
    return copy_range<string>(s | transformed(char_to_lower));
}

class anagram {
   private:
    string original_word;
    map<char, unsigned> letters;

   public:
    anagram(string word) : original_word(string_to_lower(word)) {
        for (char c : word) {
            letters[tolower(c)]++;
        }
    }

    bool match(string input) const {
        auto letters_left = letters;
        for (char c : input) {
            auto& count = letters_left[tolower(c)];
            if (count) {
                count--;
            } else {
                return false;
            }
        }
        return all_of(letters_left | map_values, _1 == 0);
    }

    set<string> matches(set<string> words) const {
        return copy_range<set<string>>(
            words | filtered([this](string const& word) {
                return string_to_lower(word) != original_word and match(word);
            }));
    }
};

}  // namespace anagram
