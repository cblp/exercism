#pragma once

#include <string>

namespace two_fer {

using namespace std;

inline string two_fer(string name = "you") {
    return "One for " + name + ", one for me.";
}

}  // namespace two_fer
