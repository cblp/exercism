#include "linked_list.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct list_node {
    struct list_node *prev, *next;
    ll_data_t data;
} list_node_t;

typedef struct list {
    list_node_t *first, *last;
} list_t;

static void list_assert_invariants(list_t* list) {
    assert(list);
    if (list->first == NULL) {
        assert(list->last == NULL);
        return;
    }
    for (list_node_t* node = list->first; node; node = node->next) {
        assert((node == list->first) == (node->prev == NULL));
        if (node != list->first) assert(node->prev && node->prev->next == node);
        assert((node == list->last) == (node->next == NULL));
    }
}

// constructs a new (empty) list
list_t* list_create(void) { return calloc(1, sizeof(list_t)); }

// counts the items on a list
size_t list_count(const list_t* list) {
    assert(list);
    size_t n = 0;
    for (list_node_t* node = list->first; node; node = node->next) ++n;
    return n;
}

// inserts item at back of a list
void list_push(list_t* list, ll_data_t data) {
    assert(list);
    list_node_t* node = calloc(1, sizeof(list_node_t));
    node->data = data;
    if (list->last) {
        node->prev = list->last;
        list->last = node;
        node->prev->next = node;
    } else {
        node->prev = node->next = NULL;
        list->first = list->last = node;
    }
    list_assert_invariants(list);
}

// removes item from back of a list
ll_data_t list_pop(list_t* list) {
    assert(list);
    assert(list->last);
    list_node_t* node = list->last;
    list->last = node->prev;
    if (list->first == node) {
        list->first = NULL;
    } else {
        list->last->next = NULL;
    }
    list_assert_invariants(list);
    ll_data_t data = node->data;
    free(node);
    return data;
}

// inserts item at front of a list
void list_unshift(list_t* list, ll_data_t data) {
    assert(list);
    list_node_t* node = calloc(1, sizeof(list_node_t));
    node->data = data;
    if (list->first) {
        node->next = list->first;
        list->first = node;
        node->next->prev = node;
    } else {
        node->prev = node->next = NULL;
        list->first = list->last = node;
    }
    list_assert_invariants(list);
}

// removes item from front of a list
ll_data_t list_shift(list_t* list) {
    assert(list);
    assert(list->first);
    list_node_t* node = list->first;
    list->first = node->next;
    if (list->last == node) {
        list->last = NULL;
    } else {
        list->first->prev = NULL;
    }
    list_assert_invariants(list);
    ll_data_t data = node->data;
    free(node);
    return data;
}

// deletes a node that holds the matching data
void list_delete(list_t* list, ll_data_t data) {
    assert(list);
    for (list_node_t* node = list->first; node; node = node->next) {
        if (node->data != data) continue;
        if (node->prev)
            node->prev->next = node->next;
        else
            list->first = node->next;
        if (node->next)
            node->next->prev = node->prev;
        else
            list->last = node->prev;
        free(node);
        break;
    }
    list_assert_invariants(list);
}

// destroys an entire list
// list will be a dangling pointer after calling this method on it
void list_destroy(list_t* list) {
    list_assert_invariants(list);
    for (list_node_t* node = list->first; node;) {
        list_node_t* next = node->next;
        free(node);
        node = next;
    }
    free(list);
}
