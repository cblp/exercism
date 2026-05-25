#pragma once

#include <functional>
#include <map>
#include <stdexcept>
#include <string>
#include <tuple>

namespace robot_simulator {

using namespace std;

enum Bearing { NORTH, EAST, SOUTH, WEST, _COUNT };

using Position = pair<int, int>;

class Robot {
   private:
    Position position;
    Bearing bearing;

   public:
    Robot() : position({0, 0}), bearing(NORTH) {}

    Robot(Position position, Bearing bearing)
        : position(position), bearing(bearing) {}

    Bearing get_bearing() const { return bearing; };

    Position get_position() const { return position; };

    void turn_left() { bearing = Bearing((bearing + 3) % _COUNT); }

    void turn_right() { bearing = Bearing((bearing + 1) % _COUNT); }

    void advance() {
        map<Bearing, function<void()>>{
            {NORTH, [&]() { position.second++; }},
            {SOUTH, [&]() { position.second--; }},
            {EAST, [&]() { position.first++; }},
            {WEST, [&]() { position.first--; }},
        }[bearing]();
    }

    void execute_sequence(string commands) {
        const map<char, function<void()>> switcher{
            {'A', bind(&Robot::advance, this)},
            {'L', bind(&Robot::turn_left, this)},
            {'R', bind(&Robot::turn_right, this)}};
        for (char command : commands) {
            switcher.at(command)();
        }
    }
};

}  // namespace robot_simulator
