#pragma once

#include <map>
#include <vector>

namespace knapsack {

using namespace std;

struct Item {
    unsigned weight;
    unsigned value;
};

inline unsigned maximum_value(unsigned maximum_weight,
                              vector<Item> const& items) {
    if (items.empty()) {
        return 0;
    }
    map<unsigned, unsigned> best{{0, 0}};
    unsigned maximum_value = 0;
    for (const auto& item : items) {
        const auto best_before_this_item = best;
        for (const auto& [best_weight, best_value] : best_before_this_item) {
            const unsigned new_weight = best_weight + item.weight;
            if (new_weight > maximum_weight) {
                continue;
            }
            unsigned const new_value = best_value + item.value;
            best[new_weight] = max(best[new_weight], new_value);
            if (new_value > maximum_value) {
                maximum_value = new_value;
            }
        }
    }
    return maximum_value;
}

}  // namespace knapsack
