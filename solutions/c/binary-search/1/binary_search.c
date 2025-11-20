#include "binary_search.h"

const int* binary_search(int value, const int* arr, size_t length) {
    if (!arr) return NULL;
    const int* left = arr;
    const int* right = arr + length - 1;
    while (left <= right) {
        const int* mid = left + (right - left) / 2;
        if (*mid == value) return mid;
        if (*mid < value)
            left = mid + 1;
        else
            right = mid - 1;
    }
    return NULL;
}
