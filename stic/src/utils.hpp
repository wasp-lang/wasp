#pragma once

#include <string>

namespace stic {
namespace utils {

bool endsWith(const std::string& str, const std::string& suffix);

std::string readFile(const std::string& filepath);

void writeFile(const char* filepath, const std::string& content);

} // namespace utils
} // namespace stic
