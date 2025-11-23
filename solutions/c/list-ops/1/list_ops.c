#include "list_ops.h"

// constructs a new list
list_t* new_list(size_t length, list_element_t elements[]) {
    list_t* list = malloc(sizeof(list_t) + length * sizeof(list_element_t));
    list->length = length;
    for (size_t i = 0; i < length; i++) {
        list->elements[i] = elements[i];
    }
    return list;
}

// append entries to a list and return the new list
list_t* append_list(list_t* list1, list_t* list2) {
    size_t new_length = list1->length + list2->length;
    list_t* new_list =
        malloc(sizeof(list_t) + new_length * sizeof(list_element_t));
    new_list->length = new_length;
    for (size_t i = 0; i < list1->length; i++) {
        new_list->elements[i] = list1->elements[i];
    }
    for (size_t j = 0; j < list2->length; j++) {
        new_list->elements[list1->length + j] = list2->elements[j];
    }
    return new_list;
}

// filter list returning only values that satisfy the filter function
list_t* filter_list(list_t* list, bool (*filter)(list_element_t)) {
    size_t count = 0;
    for (size_t i = 0; i < list->length; i++) {
        if (filter(list->elements[i])) {
            count++;
        }
    }
    list_t* filtered_list =
        malloc(sizeof(list_t) + count * sizeof(list_element_t));
    filtered_list->length = count;
    size_t index = 0;
    for (size_t i = 0; i < list->length; i++) {
        if (filter(list->elements[i])) {
            filtered_list->elements[index++] = list->elements[i];
        }
    }
    return filtered_list;
}

// returns the length of the list
size_t length_list(list_t* list) { return list->length; }

// return a list of elements whose values equal the list value transformed by
// the mapping function
list_t* map_list(list_t* list, list_element_t (*map)(list_element_t)) {
    list_t* mapped_list =
        malloc(sizeof(list_t) + list->length * sizeof(list_element_t));
    mapped_list->length = list->length;
    for (size_t i = 0; i < list->length; i++) {
        mapped_list->elements[i] = map(list->elements[i]);
    }
    return mapped_list;
}

// folds (reduces) the given list from the left with a function
list_element_t foldl_list(list_t* list, list_element_t initial,
                          list_element_t (*f)(list_element_t, list_element_t)) {
    list_element_t accumulator = initial;
    for (size_t i = 0; i < list->length; i++) {
        accumulator = f(list->elements[i], accumulator);
    }
    return accumulator;
}

// folds (reduces) the given list from the right with a function
list_element_t foldr_list(list_t* list, list_element_t initial,
                          list_element_t (*f)(list_element_t, list_element_t)) {
    list_element_t accumulator = initial;
    for (size_t i = list->length; i > 0; i--) {
        accumulator = f(list->elements[i - 1], accumulator);
    }
    return accumulator;
}

// reverse the elements of the list
list_t* reverse_list(list_t* list) {
    list_t* reversed_list =
        malloc(sizeof(list_t) + list->length * sizeof(list_element_t));
    reversed_list->length = list->length;
    for (size_t i = 0; i < list->length; i++) {
        reversed_list->elements[i] = list->elements[list->length - 1 - i];
    }
    return reversed_list;
}

// destroy the entire list
// list will be a dangling pointer after calling this method on it
void delete_list(list_t* list) { free(list); }
