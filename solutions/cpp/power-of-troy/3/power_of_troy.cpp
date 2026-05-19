#include "power_of_troy.h"

using namespace std;

namespace troy {

void give_new_artifact(human& h, string name) {
    h.possession = make_unique<artifact>(name);
}

void exchange_artifacts(unique_ptr<artifact>& a, unique_ptr<artifact>& b) {
    a.swap(b);
}

void manifest_power(human& h, string name) {
    h.own_power = make_shared<power>(name);
}

void use_power(human const& actor, human& target) {
    target.influenced_by = actor.own_power;
}

int power_intensity(human const& h) { return h.own_power.use_count(); }

}  // namespace troy
