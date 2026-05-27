#include "parallel_letter_frequency.h"

#include <tbb/blocked_range.h>
#include <tbb/parallel_reduce.h>

namespace parallel_letter_frequency {

// ~980 μs
template <typename Strings>
Result frequency_seq(Strings const& texts, Result r = {}) {
    for (const auto text : texts)
        for (const auto c : text)
            if (isalpha(c)) r[tolower(c)]++;
    return r;
}

using blocked_range = tbb::blocked_range<vector<string_view>::const_iterator>;

// ~240 μs
Result frequency(vector<string_view> const& texts) {
    return parallel_reduce(blocked_range(texts.cbegin(), texts.cend()),
                           Result{}, frequency_seq<blocked_range>, merge);
}

}  // namespace parallel_letter_frequency
