#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace secret_handshake {

using namespace std;

inline vector<string> commands(uint8_t bits) {
    vector<string> actions;
    if (bits & 1) actions.push_back("wink");
    if (bits & 2) actions.push_back("double blink");
    if (bits & 4) actions.push_back("close your eyes");
    if (bits & 8) actions.push_back("jump");
    if (bits & 16) reverse(actions.begin(), actions.end());
    return actions;
}

}  // namespace secret_handshake
