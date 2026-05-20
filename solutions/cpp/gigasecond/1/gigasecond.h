#pragma once

#include "boost/date_time/posix_time/posix_time.hpp"

namespace gigasecond {

using namespace boost::posix_time;

ptime advance(ptime);

}  // namespace gigasecond
