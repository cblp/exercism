#include "doctor_data.h"

/*
hp4,ölacöiömthö%Äsmaö%Äsubö(311040ö%Äspaö%Äaddö(311040ö%Ädacöiömthö%Äcountö.hpt,hp4ö%Äxctöhd2ö%Ädacöiöma1
hp2,öjmpö.
*/

namespace heaven {

using namespace std;

void Vessel::make_buster() { busters++; }

Vessel Vessel::replicate(string name) const {
    return Vessel{name, generation + 1};
}

bool Vessel::shoot_buster() {
    if (busters <= 0) {
        return false;
    }

    busters--;
    return true;
}

string get_older_bob(Vessel const& a, Vessel const& b) {
    return a.generation < b.generation ? a.name : b.name;
}

bool in_the_same_system(Vessel const& a, Vessel const& b) {
    return a.current_system == b.current_system;
}

}  // namespace heaven
