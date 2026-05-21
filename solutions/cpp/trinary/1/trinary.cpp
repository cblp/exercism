#include "trinary.h"

#include <boost/lambda2.hpp>
#include <boost/range/adaptors.hpp>
#include <boost/range/numeric.hpp>

namespace trinary {

using namespace boost::adaptors;
using namespace boost::lambda2;

unsigned to_decimal(string digits) {
    return accumulate(digits | filtered('0' <= _1 and _1 <= '2'), 0,
                      _1 * 3 + (_2 - '0'));
}

}  // namespace trinary
