#include <array>
#include <boost/range.hpp>
#include <boost/range/adaptor/indexed.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>
#include <string>
#include <vector>
using namespace boost::adaptors;
using namespace std;
using boost::copy_range, boost::count_if, boost::find, boost::irange;

// Round down all provided student scores.
vector<int> round_down_scores(vector<double> student_scores) {
    return copy_range<vector<int>>(student_scores);
}

// Count the number of failing students out of the group provided.
int count_failed_students(vector<int> student_scores) {
    return count_if(student_scores, [](int score) { return score <= 40; });
}

// Create a list of grade thresholds based on the provided highest grade.
array<int, 4> letter_grades(int highest_score) {
    const auto d = (highest_score - 40) / 4;
    return {41, 41 + d, 41 + d * 2, 41 + d * 3};
}

// Organize the student's rank, name, and grade information in ascending order.
vector<string> student_ranking(vector<int> student_scores,
                               vector<string> student_names) {
    return copy_range<vector<string>>(
        irange(student_names.size()) | transformed([&](const auto ix) {
            return to_string(ix + 1)
                 + ". "
                 + student_names[ix]
                 + ": "
                 + to_string(student_scores[ix]);
        }));
}

// Create a string that contains the name of the first student to make a perfect
// score on the exam.
string perfect_score(vector<int> student_scores, vector<string> student_names) {
    const auto it = find(student_scores, 100);
    const auto ix = it - begin(student_scores);
    return it == end(student_scores) ? "" : student_names[ix];
}
