#pragma once

#include <stdexcept>
#include <string>

namespace phone_number {

using namespace std;

inline void throw_wrong_format() { throw domain_error{"wrong format"}; }

class phone_number {
   private:
    string clean_number;

   public:
    phone_number(string raw_number) {
        for (char c : raw_number) {
            if (isdigit(c)) {
                clean_number += c;
            }
        }
        if (clean_number.size() < 10 or clean_number.size() > 11) {
            throw_wrong_format();
        }
        if (clean_number.size() == 11) {
            if (clean_number[0] != '1') {
                throw_wrong_format();
            }
            clean_number = clean_number.substr(1);
        }
        if (clean_number[0] < '2') {
            throw_wrong_format();
        }
        if (clean_number[3] < '2') {
            throw_wrong_format();
        }
    }

    string number() const { return clean_number; }
};

}  // namespace phone_number
