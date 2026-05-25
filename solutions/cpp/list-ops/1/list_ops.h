#pragma once

#include <functional>
#include <vector>

namespace list_ops {

using namespace std;

template <typename A>
size_t length(vector<A> const& input) {
    return input.size();
}

template <typename A>
void append(vector<A>& left, vector<A> const& right) {
    left.insert(left.end(), right.begin(), right.end());
}

template <typename A>
vector<A> concat(vector<vector<A>> const& input) {
    vector<A> result;
    for (auto const& x : input) {
        result.insert(result.end(), x.begin(), x.end());
    }
    return result;
}

template <typename A>
vector<A> reverse(vector<A> const& input) {
    vector result{input};
    for (size_t i{0}; i < result.size() / 2; ++i) {
        swap(result[i], result[result.size() - 1 - i]);
    }
    return result;
}

template <typename A, typename F,
          typename B = decltype(std::declval<F>()(std::declval<A>()))>
vector<B> map(vector<A> const& input, F const& transform) {
    vector<B> result;
    for (auto const& x : input) {
        result.push_back(transform(x));
    }
    return result;
}

template <typename A, typename F>
vector<A> filter(vector<A> const& input, F const& predicate) {
    vector<A> result;
    for (auto const& x : input) {
        if (predicate(x)) {
            result.push_back(x);
        }
    }
    return result;
}

template <typename A, typename B, typename F>
B foldl(vector<A> const& input, B const& init, F const& f) {
    B result{init};
    for (auto const& x : input) {
        result = f(result, x);
    }
    return result;
}

template <typename A, typename B, typename F>
B foldr(vector<A> const& input, B const& init, F const& f) {
    B result{init};
    for (size_t i{input.size()}; i > 0; --i) {
        result = f(result, input[i - 1]);
    }
    return result;
}

}  // namespace list_ops
