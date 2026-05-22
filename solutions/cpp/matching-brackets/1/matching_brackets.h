#pragma once

#include <string>

namespace matching_brackets {

using namespace std;

inline char opening(char c) {
    switch (c) {
        case ')':
            return '(';
        case '}':
            return '{';
        case ']':
            return '[';
        default:
            return 0;
    }
}

inline bool check(string input) {
    string stack;
    for (char c : input) {
        if (c == '{' or c == '[' or c == '(') {
            stack += c;
        } else if (const auto op = opening(c)) {
            if (not stack.empty() and op == stack.back()) {
                stack.pop_back();
            } else {
                return false;
            }
        }
    }
    return stack.empty();
}

}  // namespace matching_brackets
