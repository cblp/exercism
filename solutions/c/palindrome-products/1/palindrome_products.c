#include "palindrome_products.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static factor_t* new_factors(int const a, int const b) {
    factor_t* r = calloc(1, sizeof(factor_t));
    r->factor_a = a;
    r->factor_b = b;
    return r;
}

static factor_t* add_factors(int const a, int const b, factor_t* const next) {
    factor_t* r = malloc(sizeof(factor_t));
    r->factor_a = a;
    r->factor_b = b;
    r->next = next;
    return r;
}

static void free_factors(factor_t* f) {
    while (f) {
        factor_t* next = f->next;
        free(f);
        f = next;
    }
}

static bool is_palindrome(int x) {
    if (x % 10 == 0) {
        return false;
    }
    char s[10] = {0};
    snprintf(s, sizeof(s), "%d", x);
    size_t const len = strlen(s);
    assert(len < sizeof(s));
    for (size_t i = 0; i < len / 2; i++) {
        if (s[i] != s[len - 1 - i]) {
            return false;
        }
    }
    return true;
}

static void update(product_t* r, int const a, int const b) {
    int const value = a * b;
    if (!r->factors_sm)  // is empty
    {
        r->smallest = value;
        r->largest = value;
        r->factors_sm = new_factors(a, b);
        r->factors_lg = new_factors(a, b);
    } else {
        if (value < r->smallest) {
            r->smallest = value;
            free_factors(r->factors_sm);
            r->factors_sm = new_factors(a, b);
        } else if (value == r->smallest) {
            r->factors_sm = add_factors(a, b, r->factors_sm);
        }
        if (value > r->largest) {
            r->largest = value;
            free_factors(r->factors_lg);
            r->factors_lg = new_factors(a, b);
        } else if (value == r->largest) {
            r->factors_lg = add_factors(a, b, r->factors_lg);
        }
    }
}

product_t* get_palindrome_product(int min, int max) {
    product_t* r = calloc(1, sizeof(product_t));
    if (min > max) {
        snprintf(r->error, MAXERR, "invalid input: min is %d and max is %d",
                 min, max);
        return r;
    }
    for (int a = min; a <= max; a++) {
        for (int b = a; b <= max; b++) {
            int const value = a * b;
            if (!is_palindrome(value)) {
                continue;
            }
            update(r, a, b);
        }
    }
    if (!r->factors_sm)  // is empty
    {
        snprintf(r->error, MAXERR,
                 "no palindrome with factors in the range %d to %d", min, max);
    }
    return r;
}

void free_product(product_t* p) { free(p); }
