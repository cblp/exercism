#pragma once

namespace leap {

inline bool is_leap_year(int y) {
    return not(y % 4) and ((y % 100) or not(y % 400));
}

}  // namespace leap
