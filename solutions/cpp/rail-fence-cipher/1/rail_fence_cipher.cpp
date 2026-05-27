#include "rail_fence_cipher.h"

namespace rail_fence_cipher {

using namespace std;

template <bool is_decoding>
string subst(string const& text, int rails) {
    const auto max_step = 2 * (rails - 1);
    string out(text.size(), ' ');
    int cipher_ix = 0;
    for (int r = 0; r < rails; ++r) {
        int step = 2 * r;
        size_t plain_ix = r;
        while (plain_ix < text.size()) {
            if constexpr (is_decoding) {
                out[plain_ix] = text[cipher_ix];
            } else {
                out[cipher_ix] = text[plain_ix];
            }
            ++cipher_ix;
            if (step != max_step) {
                step = max_step - step;
            }
            plain_ix += step;
        }
    }
    return out;
}

string encode(string const& text, int rails) {
    return subst<false>(text, rails);
}

string decode(string const& text, int rails) {
    return subst<true>(text, rails);
}

}  // namespace rail_fence_cipher
