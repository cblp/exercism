#pragma once

#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm.hpp>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace grade_school {

using namespace boost;
using namespace boost::adaptors;
using namespace std;

class school {
   private:
    map<int, set<string>> _roster;

   public:
    void add(string name, int grade) { _roster[grade].insert(name); }

    vector<string> grade(int g) const {
        if (const auto it = _roster.find(g); it != _roster.end()) {
            return copy_range<vector<string>>(it->second);
        }
        return {};
    }

    map<int, vector<string>> roster() const {
        return copy_range<map<int, vector<string>>>(
            _roster | transformed([](const auto kv) {
                const auto [k, v] = kv;
                return pair(k, copy_range<vector<string>>(v));
            }));
    }
};

}  // namespace grade_school
