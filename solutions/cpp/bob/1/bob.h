#pragma once

#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/range/adaptors.hpp>
#include <functional>
#include <string>

namespace bob {

using namespace boost;
using namespace boost::adaptors;
using namespace boost::algorithm;
using namespace std;

inline bool is_letter(char c) { return isalpha(c); }

inline bool is_upper(char c) { return isupper(c); }

inline bool is_space(char c) { return isspace(c); }

inline string hey(string input) {
    input = copy_range<string>(input | filtered(not_fn(is_space)));
    const auto letters = copy_range<string>(input | filtered(is_letter));
    const auto is_question = input.back() == '?';
    const auto is_yelling = not letters.empty() and all_of(letters, is_upper);
    if (input.empty()) return "Fine. Be that way!";
    if (is_question and is_yelling) return "Calm down, I know what I'm doing!";
    if (is_question) return "Sure.";
    if (is_yelling) return "Whoa, chill out!";
    return "Whatever.";
}

}  // namespace bob
