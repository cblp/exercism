#include "lasagna_master.h"

#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm.hpp>

using namespace boost::adaptors;
using boost::copy_range;
using namespace std;

namespace lasagna_master {

int preparationTime(vector<string> layers, int oneLayerTime) {
    return layers.size() * oneLayerTime;
}

amount quantities(vector<string> layers) {
    amount res{};
    for (const auto& layer : layers) {
        if (layer == "noodles") {
            res.noodles += 50;
        } else if (layer == "sauce") {
            res.sauce += 0.2;
        }
    }
    return res;
}

void addSecretIngredient(vector<string>& myList, vector<string> friendsList) {
    for (auto& ingredient : myList) {
        if (ingredient == "?") {
            ingredient = friendsList.back();
            return;
        }
    }
}

void addSecretIngredient(vector<string>& myList, string secret) {
    addSecretIngredient(myList, vector{secret});
}

vector<double> scaleRecipe(vector<double> input, int portions) {
    return copy_range<vector<double>>(input | transformed([&](double amount) {
                                          return amount * portions / 2;
                                      }));
}

}  // namespace lasagna_master
