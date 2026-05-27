#include "parallel_letter_frequency.h"

#include <tbb/blocked_range.h>
#include <tbb/parallel_reduce.h>

namespace parallel_letter_frequency {

// ~980 μs
Result frequency_seq(vector<string_view> const& texts) {
    Result freqs;
    for (const auto text : texts)
        for (const auto c : text)
            if (isalpha(c)) freqs[tolower(c)]++;
    return freqs;
}

using blocked_range = tbb::blocked_range<vector<string_view>::const_iterator>;

// ~240 μs
Result frequency(vector<string_view> const& texts) {
    return parallel_reduce(
        blocked_range(texts.cbegin(), texts.cend()), Result{},
        [](const blocked_range& texts, Result r) {
            for (auto text : texts)
                for (const auto c : text)
                    if (isalpha(c)) r[tolower(c)]++;
            return r;
        },
        [](Result a, const Result& b) {
            for (const auto& [c, n] : b) a[c] += n;
            return a;
        });
}

}  // namespace parallel_letter_frequency
