#include "matching_brackets.h"

char opening_bracket(char c);
char opening_bracket(char c) {
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

bool is_paired(const char* input) {
    char stack[1024];
    int top = -1;

    for (const char* p = input; *p; p++) {
        char const c = *p;
        char const o = opening_bracket(c);
        if (c == '(' || c == '{' || c == '[') {
            stack[++top] = c;
        } else if (o) {
            if (top < 0 || stack[top] != o) {
                return false;
            }
            top--;
        }
    }

    return top == -1;
}
