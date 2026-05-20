#pragma once

#include <iomanip>
#include <sstream>
#include <string>

namespace date_independent {

using namespace std;

inline int mod(int a, int n) {
    const int r = a % n;
    return r < 0 ? r + n : r;
}

class clock {
   private:
    unsigned minutes;

    clock(unsigned minutes) : minutes(mod(minutes, 24 * 60)) {}

   public:
    static clock at(int hours, int minutes) {
        return clock(hours * 60 + minutes);
    }

    bool operator==(clock other) const { return minutes == other.minutes; }

    bool operator!=(clock other) const { return minutes != other.minutes; }

    operator string() const {
        return (stringstream()
                << setw(2)
                << setfill('0')
                << (minutes / 60) % 24
                << ":"
                << setw(2)
                << setfill('0')
                << minutes % 60)
            .str();
    }

    clock plus(int minutes) const { return clock(this->minutes + minutes); }
};

}  // namespace date_independent
