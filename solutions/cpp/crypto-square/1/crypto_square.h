#pragma once

#include <boost/algorithm/string.hpp>
#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <string>
#include <vector>

namespace crypto_square {

using namespace boost;
using namespace boost::adaptors;
using namespace std;

struct cipher {
    string plain_text;

    cipher(string plain_text) : plain_text(plain_text) {}

    string normalized_cipher_text() const {
        const string message = copy_range<string>(
            plain_text
            | filtered([](char c) { return isalnum(c); })
            | transformed([](char c) { return tolower(c); }));

        if (message.empty()) {
            return "";
        }

        size_t cols;
        for (size_t rows = 0, kontinue = true; kontinue; rows++) {
            for (cols = rows; cols <= rows + 1; cols++) {
                if (rows * cols >= message.size()) {
                    kontinue = false;
                    break;
                }
            }
        }

        vector<string> plain_square;
        for (size_t i = 0; i < message.size(); i += cols) {
            plain_square.push_back(message.substr(i, cols));
        }
        plain_square.back() += string(cols - plain_square.back().size(), ' ');

        vector<string> cipher_square;
        for (size_t col = 0; col < cols; col++) {
            string cipher_line;
            for (const auto& plain_line : plain_square) {
                cipher_line += plain_line[col];
            }
            cipher_square.push_back(cipher_line);
        }

        return join(cipher_square, " ");
    }
};

}  // namespace crypto_square
