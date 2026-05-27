#pragma once

#include <boost/algorithm/cxx11/any_of.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/tokenizer.hpp>
#include <string>
#include <vector>

namespace pig_latin {

using namespace boost;
using namespace boost::algorithm;
using namespace std;

inline string translate_word(string word) {
    if (any_of(vector{"xr", "yt", "a", "e", "i", "o", "u"}, [&](string prefix) {
            return word.substr(0, prefix.size()) == prefix;
        })) {
        return word + "ay";
    }
    if (word.substr(0, 2) == "qu") {
        return word.substr(2) + "quay";
    }
    if (word.size() > 2 && word.substr(1, 2) == "qu") {
        return word.substr(3) + word[0] + "quay";
    }
    if (word.size() > 1) {
        auto pos = word.find_first_of("aeiouy", 1);
        if (pos != string::npos) {
            return word.substr(pos) + word.substr(0, pos) + "ay";
        }
    }
    throw word;
}

inline string translate(string words) {
    vector<string> result;
    for (auto word : tokenizer(words, char_separator(" "))) {
        result.push_back(translate_word(word));
    }
    return join(result, " ");
}

}  // namespace pig_latin
