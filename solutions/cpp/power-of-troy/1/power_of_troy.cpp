#include "power_of_troy.h"

namespace troy {

void give_new_artifact(human& h, std::string name) {
    h.possession.reset(new artifact{name});
}

void exchange_artifacts(std::unique_ptr<artifact>& a,
                        std::unique_ptr<artifact>& b) {
    a.swap(b);
}

void manifest_power(human& h, std::string name) {
    h.own_power.reset(new power{name});
}

void use_power(human const& actor, human& target) {
    target.influenced_by = actor.own_power;
}

int power_intensity(human const& h) { return h.own_power.use_count(); }

}  // namespace troy
