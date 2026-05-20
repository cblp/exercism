#pragma once

#include <string>

namespace robot_name {

using namespace std;

class robot {
   private:
    static unsigned counter;

    string _name;

   public:
    robot() { reset(); }

    string name() const { return _name; }

    void reset() {
        char buf[] = "AA000";
        snprintf(buf, sizeof buf, "A%c%03d", 'A' + counter / 1000,
                 counter % 1000);
        _name = buf;
        counter++;
    }
};

}  // namespace robot_name
