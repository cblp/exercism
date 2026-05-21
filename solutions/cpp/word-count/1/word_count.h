#pragma once

#include <map>
#include <string>

namespace word_count {

using namespace std;

inline map<string, int> words(string sentence) {
    map<string, int> words;
    string current_word;
    bool apostrophe = false;
    for (char c : sentence) {
        if (isalpha(c) || isdigit(c)) {
            if (apostrophe) {
                current_word += '\'';
                apostrophe = false;
            }
            current_word += tolower(c);
        } else if (c == '\'' and not apostrophe and not current_word.empty()) {
            apostrophe = true;
        } else {
            apostrophe = false;
            if (not current_word.empty()) {
                words[current_word]++;
                current_word.clear();
            }
        }
    }
    if (not current_word.empty()) {
        words[current_word]++;
    }
    return words;
}

}  // namespace word_count
