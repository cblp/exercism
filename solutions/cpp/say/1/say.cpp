#include "say.h"

#include <vector>
using namespace std;

#include <boost/algorithm/string.hpp>
#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
using namespace boost;
using namespace boost::adaptors;

namespace say {

const string ONES[]{"one", "two",   "three", "four", "five",
                    "six", "seven", "eight", "nine"};

const string TENS[]{"ten",   "twenty",  "thirty", "forty", "fifty",
                    "sixty", "seventy", "eighty", "ninety"};

const string TEENS[]{"eleven",  "twelve",    "thirteen", "fourteen", "fifteen",
                     "sixteen", "seventeen", "eighteen", "nineteen"};

vector<string> say_group(string digits) {
    vector<string> res;
    const unsigned digit0 = digits.size() > 0 ? digits[0] - '0' : 0;
    const unsigned digit1 = digits.size() > 1 ? digits[1] - '0' : 0;
    if (digit0) {
        switch (digit1) {
            case 0:
                res.push_back(ONES[digit0 - 1]);
                break;
            case 1:
                res.push_back(TEENS[digit0 - 1]);
                break;
            default:
                res.push_back(TENS[digit1 - 1] + "-" + ONES[digit0 - 1]);
        }
    } else if (digit1) {
        res.push_back(TENS[digit1 - 1]);
    }
    const unsigned digit2 = digits.size() > 2 ? digits[2] - '0' : 0;
    if (digit2) {
        res.push_back("hundred");
        res.push_back(ONES[digit2 - 1]);
    }
    return res;
}

vector<string> say_parts(string digits) {
    vector<string> res;

    copy(say_group(digits.substr(0, 3)), back_inserter(res));

    const vector<string> groups{"thousand", "million", "billion"};
    for (size_t i = 0; i < groups.size(); i++) {
        if (digits.size() <= 3 * (i + 1)) {
            break;
        }
        const auto group = say_group(digits.substr(3 * (i + 1), 3));
        if (not group.empty()) {
            res.push_back(groups[i]);
            copy(group, back_inserter(res));
        }
    }

    return res;
}

string in_english(long n) {
    if (n < 0 or n >= 1'000'000'000'000) {
        throw domain_error{"n in [0 .. 1'000'000'000'000]"};
    }
    if (n == 0) {
        return "zero";
    }
    return join(
        say_parts(copy_range<string>(to_string(n) | reversed)) | reversed, " ");
}

}  // namespace say
