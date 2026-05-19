#pragma once

#include <string>
#include <vector>

namespace lasagna_master {

struct amount {
    int noodles;
    double sauce;
};

int preparationTime(std::vector<std::string> layers, int oneLayerTime = 2);

amount quantities(std::vector<std::string> layers);

void addSecretIngredient(std::vector<std::string>& myList,
                         std::vector<std::string> friendsList);

void addSecretIngredient(std::vector<std::string>& myList, std::string secret);

std::vector<double> scaleRecipe(std::vector<double> input, int portions);

}  // namespace lasagna_master
