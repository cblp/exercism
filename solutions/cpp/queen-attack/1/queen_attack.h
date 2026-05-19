#pragma once

#include <stdexcept>
#include <string>
#include <tuple>

namespace queen_attack {

using namespace std;

using pos = pair<int, int>;

inline void assert_valid(int i) {
    if (i < 0 or i >= 8) {
        throw domain_error{to_string(i)};
    }
}

inline void assert_valid(pos p) {
    assert_valid(p.first);
    assert_valid(p.second);
}

class chess_board {
   private:
    pos _white, _black;

   public:
    chess_board(pos white, pos black) : _white(white), _black(black) {
        assert_valid(white);
        assert_valid(black);
        if (white == black) {
            throw domain_error{"white = black"};
        }
    }

    pos white() const { return _white; }

    pos black() const { return _black; }

    bool can_attack() const {
        return _white.first == _black.first
            or _white.second == _black.second
            or _white.first + _white.second == _black.first + _black.second
            or _white.first - _white.second == _black.first - _black.second;
    }
};

}  // namespace queen_attack
