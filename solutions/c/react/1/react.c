#include "react.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct cell {
    int value;
    struct cell* input1;
    struct cell* input2;
    compute1_t compute1;
    compute2_t compute2;
} cell_t;

#define MAX_CELLS 10
#define MAX_CALLBACKS 10

typedef struct callback_store {
    cell_t* cell;
    callback_t callback;
    void* env;
} callback_store_t;

typedef struct reactor {
    cell_t cells[MAX_CELLS];
    size_t cells_count;

    callback_store_t callbacks[MAX_CALLBACKS];
    size_t callbacks_count;
} reactor_t;

reactor_t REACTOR;

reactor_t* create_reactor(void) {
    memset(&REACTOR, 0, sizeof(REACTOR));
    return &REACTOR;
}

// destroy_reactor should free all cells created under that reactor.
void destroy_reactor(reactor_t* reactor) { assert(reactor == &REACTOR); }

cell_t* create_input_cell(reactor_t* reactor, int initial_value) {
    assert(reactor == &REACTOR);
    assert(reactor->cells_count < MAX_CELLS);
    cell_t* cell = &reactor->cells[reactor->cells_count++];
    cell->value = initial_value;
    return cell;
}

cell_t* create_compute1_cell(reactor_t* const reactor, cell_t* const input1,
                             compute1_t const compute1) {
    assert(reactor == &REACTOR);
    assert(input1);
    assert(compute1);

    assert(reactor->cells_count < MAX_CELLS);
    cell_t* cell = &reactor->cells[reactor->cells_count];
    reactor->cells_count++;

    cell->input1 = input1;
    cell->compute1 = compute1;
    cell->value = compute1(input1->value);

    return cell;
}

cell_t* create_compute2_cell(reactor_t* const reactor, cell_t* const input1,
                             cell_t* const input2, compute2_t const compute2) {
    assert(reactor == &REACTOR);
    assert(input1);
    assert(input2);
    assert(compute2);

    assert(reactor->cells_count < MAX_CELLS);
    cell_t* cell = &reactor->cells[reactor->cells_count];
    reactor->cells_count++;

    cell->input1 = input1;
    cell->input2 = input2;
    cell->compute2 = compute2;
    cell->value = compute2(input1->value, input2->value);

    return cell;
}

int get_cell_value(cell_t* const cell) {
    assert(cell);
    return cell->value;
}

static void update_and_trigger_callbacks(cell_t* const cell, bool is_dirty[],
                                         int const new_value) {
    if (cell->value == new_value) {
        return;
    }
    cell->value = new_value;
    is_dirty[cell - REACTOR.cells] = true;
    for (size_t i = 0; i < REACTOR.callbacks_count; i++) {
        callback_store_t* const cb = &REACTOR.callbacks[i];
        if (cb->cell == cell) {
            cb->callback(cb->env, new_value);
        }
    }
}

static void recompute(cell_t* const cell, bool is_dirty[]) {
    int new_value;
    if (cell->compute1 && is_dirty[cell->input1 - REACTOR.cells]) {
        new_value = cell->compute1(cell->input1->value);
    } else if (cell->compute2 && (is_dirty[cell->input1 - REACTOR.cells] ||
                                  is_dirty[cell->input2 - REACTOR.cells])) {
        new_value = cell->compute2(cell->input1->value, cell->input2->value);
    } else {
        return;
    }
    update_and_trigger_callbacks(cell, is_dirty, new_value);
}

void set_cell_value(cell_t* const cell, int const new_value) {
    assert(cell);

    bool is_dirty[MAX_CELLS] = {0};
    update_and_trigger_callbacks(cell, is_dirty, new_value);
    for (size_t i = cell - REACTOR.cells + 1; i < REACTOR.cells_count; i++) {
        recompute(&REACTOR.cells[i], is_dirty);
    }
}

// The callback should be called with the same void * given in add_callback.
callback_id add_callback(cell_t* const cell, void* const env,
                         callback_t const callback) {
    assert(cell);
    assert(callback);

    assert(REACTOR.callbacks_count < MAX_CALLBACKS);
    callback_id const id = REACTOR.callbacks_count;
    REACTOR.callbacks[id].cell = cell;
    REACTOR.callbacks[id].callback = callback;
    REACTOR.callbacks[id].env = env;
    REACTOR.callbacks_count++;

    return id;
}

void remove_callback(cell_t* const cell, callback_id const id) {
    assert(!REACTOR.callbacks[id].cell || REACTOR.callbacks[id].cell == cell);
    REACTOR.callbacks[id].cell = NULL;
}
