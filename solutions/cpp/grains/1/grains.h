#pragma once

#include <cstdint>
#include <limits>

namespace grains {

inline uint64_t square(uint8_t x) {
    return static_cast<uint64_t>(1) << (x - 1);
}

inline constexpr uint64_t total() {
    return std::numeric_limits<uint64_t>::max();
}

}  // namespace grains
