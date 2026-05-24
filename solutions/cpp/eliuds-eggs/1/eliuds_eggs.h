#pragma once

namespace chicken_coop {

inline unsigned positions_to_quantity(unsigned p) {
    unsigned n = 0;
    while (p) {
        n += p & 1;
        p >>= 1;
    }
    return n;
}

}  // namespace chicken_coop
