#include "high_scores.h"

#include <numeric>

namespace arcade {

using namespace std;

vector<int> HighScores::list_scores() { return scores; }

int HighScores::latest_score() { return scores.back(); }

int HighScores::personal_best() {
    return accumulate(scores.begin(), scores.end(), 0,
                      [](auto a, auto b) { return max(a, b); });
}

vector<int> HighScores::top_three() {
    vector<int> top;
    auto scores_copy = scores;
    make_heap(scores_copy.begin(), scores_copy.end());
    for (int i = 0; i < 3 && !scores_copy.empty(); ++i) {
        pop_heap(scores_copy.begin(), scores_copy.end());
        top.push_back(scores_copy.back());
        scores_copy.pop_back();
    }
    return top;
}

}  // namespace arcade
