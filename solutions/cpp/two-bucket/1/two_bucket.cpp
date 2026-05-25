#include "two_bucket.h"

#include <queue>
#include <set>

namespace two_bucket {

using namespace std;

using BucketsState = pair<int, int>;

struct State {
    BucketsState buckets;
    int move_count;
};

#define add(_b1, _b2)                                       \
    do {                                                    \
        bool const stateIsForbidden =                       \
            (start_bucket == bucket_id::one                 \
                 ? _b1 == 0 and _b2 == bucket2_capacity     \
                 : _b1 == bucket1_capacity and _b2 == 0);   \
        BucketsState const bs = {_b1, _b2};                 \
        if (not visited.count(bs) and not stateIsForbidden) \
            q.push(State{bs, move_count + 1});              \
    } while (0)

measure_result measure(int const bucket1_capacity, int const bucket2_capacity,
                       int const target_volume, bucket_id const start_bucket) {
    set<BucketsState> visited;
    queue<State> q;
    {
        State startState;
        startState.move_count = 1;
        switch (start_bucket) {
            case bucket_id::one:
                startState.buckets.first = bucket1_capacity;
                break;
            case bucket_id::two:
                startState.buckets.second = bucket2_capacity;
                break;
        }
        q.push(startState);
    }
    while (not q.empty()) {
        State const state = q.front();
        q.pop();
        int const move_count = state.move_count;
        BucketsState const buckets = state.buckets;
        int const bucket_1 = state.buckets.first;
        int const bucket_2 = state.buckets.second;

        // check if state is goal
        if (bucket_1 == target_volume) {
            return measure_result{state.move_count, bucket_id::one,
                                  state.buckets.second};
        }
        if (bucket_2 == target_volume) {
            return measure_result{state.move_count, bucket_id::two,
                                  state.buckets.first};
        }
        visited.insert(buckets);

        // build next moves
        // fill one
        if (bucket_1 < bucket1_capacity) {
            // pour two -> one
            if (bucket_2 > 0) {
                int const d = min(bucket1_capacity - bucket_1, bucket_2);
                add(bucket_1 + d, bucket_2 - d);
            }
            add(bucket1_capacity, bucket_2);
        }
        // fill two
        if (bucket_2 < bucket2_capacity) {
            // pour one -> two
            if (bucket_1 > 0) {
                int const d = min(bucket2_capacity - bucket_2, bucket_1);
                add(bucket_1 - d, bucket_2 + d);
            }
            add(bucket_1, bucket2_capacity);
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
    throw nullptr;
}

}  // namespace two_bucket
