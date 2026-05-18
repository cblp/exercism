#pragma once

#include <string>

/*
hp1, üapöhp2ö % Äcountöiöma1,
    öhp2ö % Älawöhp3öö / önextöstepö % Ädacöiöml1ö % Älawö7ö % Ädacöiömb1ö %
        Ärandomöö % Äscrö9sö % Äsirö9sö % Äxctöhr1ö % Äaddöiömx1ö %
        Ädacöiömx1ö % Äswapö % Äaddöiömy1ö % Ädacöiömy1ö % Ärandomö % Äscrö9sö %
        Äsirö9sö % Äxctöhr2ö % Ädacöiömdyö % Ädioöiömdxö % Äsetupö.hpt,
    3ö % Älacöranö % Ädacöiömth
*/

namespace star_map {

enum class System {
    AlphaCentauri,
    BetaHydri,
    DeltaEridani,
    EpsilonEridani,
    Omicron2Eridani,
    Sol,
};

}  // namespace star_map

namespace heaven {

struct Vessel {
    std::string name;
    int generation;

    star_map::System current_system = star_map::System::Sol;
    int busters = 0;

    void make_buster();

    Vessel replicate(std::string name) const;

    bool shoot_buster();
};

std::string get_older_bob(Vessel const& a, Vessel const& b);

bool in_the_same_system(Vessel const& a, Vessel const& b);

}  // namespace heaven
