#include "gigasecond.h"

namespace gigasecond {

ptime advance(ptime input) {
    return input + time_duration(0, 0, 1'000'000'000);
}

}  // namespace gigasecond
