#include "two_bucket.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

static int min(int const a, int const b) { return a < b ? a : b; }

// BucketsState ----------------------------------------------------------------

typedef struct {
    bucket_liters_t bucket_1;
    bucket_liters_t bucket_2;
} BucketsState;

static bool BucketsState_eq(BucketsState const a, BucketsState const b) {
    return memcmp(&a, &b, sizeof(BucketsState)) == 0;
}

// SetOfBucketsStates ----------------------------------------------------------

#define SetOfBucketsStates_capacity 30
typedef struct {
    // impl: eq-set
    BucketsState data[SetOfBucketsStates_capacity];
    size_t size;
} SetOfBucketsStates;

static bool SetOfBucketsStates_contains(SetOfBucketsStates* const set,
                                        BucketsState const state) {
    for (size_t i = 0; i < set->size; i++)
        if (BucketsState_eq(set->data[i], state)) return true;
    return false;
}

static void SetOfBucketsStates_add(SetOfBucketsStates* const set,
                                   BucketsState const state) {
    for (size_t i = 0; i < set->size; i++) {
        if (BucketsState_eq(set->data[i], state)) {
            return;
        }
    }
    assert(set->size < SetOfBucketsStates_capacity);
    set->data[set->size] = state;
    set->size++;
}

// State -----------------------------------------------------------------------

typedef struct {
    BucketsState buckets;
    size_t move_count;
} State;

// QueueOfStates ---------------------------------------------------------------

#define QueueOfStates_physicalCapacity 10
typedef struct {
    // imple: ring buffer
    State data[QueueOfStates_physicalCapacity];
    size_t reader;
    size_t writer;
} QueueOfStates;

static void _QueueOfStates_advanceReader(QueueOfStates* q) {
    assert(q);
    q->reader = (q->reader + 1) % QueueOfStates_physicalCapacity;
}

static void _QueueOfStates_writeAndAdvanceWriter(QueueOfStates* q,
                                                 State const s) {
    assert(q);
    q->data[q->writer] = s;
    q->writer = (q->writer + 1) % QueueOfStates_physicalCapacity;
}

static bool QueueOfStates_isEmpty(QueueOfStates* q) {
    assert(q);
    return q->reader == q->writer;
}

static bool QueueOfStates_isFull(QueueOfStates* q) {
    assert(q);
    return q->reader == (q->writer + 1) % QueueOfStates_physicalCapacity;
}

static void QueueOfStates_push(QueueOfStates* const q, State const s) {
    assert(q);
    assert(!QueueOfStates_isFull(q));
    _QueueOfStates_writeAndAdvanceWriter(q, s);
}

static State QueueOfStates_pop(QueueOfStates* q) {
    assert(q);
    assert(!QueueOfStates_isEmpty(q));
    State const s = q->data[q->reader];
    _QueueOfStates_advanceReader(q);
    return s;
}

// measure ---------------------------------------------------------------------

static bucket_result_t resultPossible(bucket_id_t const goal_bucket,
                                      State const s) {
    return (bucket_result_t){
        .possible = true,
        .move_count = s.move_count,
        .goal_bucket = goal_bucket,
        .other_bucket_liters = goal_bucket == BUCKET_ID_1 ? s.buckets.bucket_2
                                                          : s.buckets.bucket_1,
    };
}

#define add(_b1, _b2)                                                         \
    do {                                                                      \
        bool const stateIsForbidden =                                         \
            (start_bucket == BUCKET_ID_1 ? _b1 == 0 && _b2 == bucket_2_size   \
                                         : _b1 == bucket_1_size && _b2 == 0); \
        BucketsState const bs = {_b1, _b2};                                   \
        if (!SetOfBucketsStates_contains(&visited, bs) && !stateIsForbidden)  \
            QueueOfStates_push(&q, (State){                                   \
                                       .move_count = move_count + 1,          \
                                       .buckets = bs,                         \
                                   });                                        \
    } while (0)

bucket_result_t measure(bucket_liters_t const bucket_1_size,
                        bucket_liters_t const bucket_2_size,
                        bucket_liters_t const goal_volume,
                        bucket_id_t const start_bucket) {
    SetOfBucketsStates visited = {0};
    QueueOfStates q = {0};
    {
        State startState = {0};
        startState.move_count = 1;
        switch (start_bucket) {
            case BUCKET_ID_1:
                startState.buckets.bucket_1 = bucket_1_size;
                break;
            case BUCKET_ID_2:
                startState.buckets.bucket_2 = bucket_2_size;
                break;
        }
        QueueOfStates_push(&q, startState);
    }
    while (!QueueOfStates_isEmpty(&q)) {
        State const state = QueueOfStates_pop(&q);
        unsigned const move_count = state.move_count;
        BucketsState const buckets = state.buckets;
        bucket_liters_t const bucket_1 = state.buckets.bucket_1;
        bucket_liters_t const bucket_2 = state.buckets.bucket_2;

        // check if state is goal
        if (bucket_1 == goal_volume) {
            return resultPossible(BUCKET_ID_1, state);
        }
        if (bucket_2 == goal_volume) {
            return resultPossible(BUCKET_ID_2, state);
        }
        SetOfBucketsStates_add(&visited, buckets);

        // build next moves
        // fill one
        if (bucket_1 < bucket_1_size) {
            // pour two -> one
            if (bucket_2 > 0) {
                int const d = min(bucket_1_size - bucket_1, bucket_2);
                add(bucket_1 + d, bucket_2 - d);
            }
            add(bucket_1_size, bucket_2);
        }
        // fill two
        if (bucket_2 < bucket_2_size) {
            // pour one -> two
            if (bucket_1 > 0) {
                int const d = min(bucket_2_size - bucket_2, bucket_1);
                add(bucket_1 - d, bucket_2 + d);
            }
            add(bucket_1, bucket_2_size);
        }
        // empty one
        if (bucket_1 > 0) {
            add(0, bucket_2);
        }
        // empty two
        if (bucket_2 > 0) {
            add(bucket_1, 0);
        }
    }
    return (bucket_result_t){0};
}
