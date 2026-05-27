#pragma once

#include <string>

namespace affine_cipher {

std::string encode(const std::string& input, unsigned a, unsigned b);

std::string decode(const std::string& input, unsigned a, unsigned b);

}  // namespace affine_cipher
