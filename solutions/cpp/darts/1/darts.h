#pragma once

namespace darts {

inline unsigned score(float x, float y) {
    const auto d2 = x * x + y * y;
    return d2 <= 1 ? 10 : d2 <= 25 ? 5 : d2 <= 100 ? 1 : 0;
}

}  // namespace darts
