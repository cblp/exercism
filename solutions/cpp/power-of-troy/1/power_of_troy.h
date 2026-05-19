#pragma once

#include <memory>
#include <string>

namespace troy {

struct artifact {
    std::string name;
};

struct power {
    std::string effect;
};

struct human {
    std::unique_ptr<artifact> possession;
    std::shared_ptr<power> own_power;
    std::shared_ptr<power> influenced_by;
};

void give_new_artifact(human& h, std::string name);

void exchange_artifacts(std::unique_ptr<artifact>& a,
                        std::unique_ptr<artifact>& b);

void manifest_power(human& h, std::string name);

void use_power(human const& actor, human& target);

int power_intensity(human const& h);

}  // namespace troy
