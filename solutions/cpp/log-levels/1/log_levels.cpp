#include <string>

namespace log_line {
std::string message(std::string line) {
    const auto delim = line.find("]: ");
    return line.substr(delim + 3);
}

std::string log_level(std::string line) {
    const auto delim = line.find("]: ");
    return line.substr(1, delim - 1);
}

std::string reformat(std::string line) {
    return message(line) + " (" + log_level(line) + ")";
}
}  // namespace log_line
