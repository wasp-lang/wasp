#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <regex>
#include <iterator>

#include "./hello-world.hpp" // TODO: redundant, remove.

bool endsWith(const std::string& str, const std::string& suffix) {
    return str.length() >= suffix.length() && 0 == str.compare(str.length() - suffix.length(), suffix.length(), suffix);
}

std::string readFile(const std::string& filepath) {
    std::ifstream file(filepath);
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

void writeFile(const char* filepath, const std::string& content) {
    std::ofstream file(filepath);
    if (file.is_open()) {
        file << content;
        file.close();
    }
}

void printHelp() {
    std::cout << "Usage: stick-cli <your-spec-file>.wasp" << std::endl;
    std::cout << "Currently, only 'page' is supported (in wasp)." << std::endl;
}

// TODO(martin): We should probably use Boost (or some other library), to make our lives easier.

void buildWebAppFromWasp(std::string waspFileContent) {
    // NOTE: This is super primitive implementation, just to get us going.
    // What would be nicer is to build Specification (or Wasp) object from the wasp file,
    // and then build further from it (either to some other intermediate representation or directly generate files).

    // Read page name.
    std::string pageName;
    std::regex pageRegex("page \"([^\"]+)\"");
    std::smatch matches;
    if (std::regex_search(waspFileContent, matches, pageRegex)) {
        pageName = matches[1].str();
    }

    std::string indexHtml = "<html> <head> <title>" + pageName + "</title> </head> <body> Welcome to my \"" + pageName + "\" page! </body> </html>";

    writeFile("index.html", indexHtml);
}

int main (int argc, char* argv[]) {

    // Check parameters.
    if (argc != 2) { printHelp(); return 1; }
    // First parameter should be .wasp file path.
    const std::string waspFilepath(argv[1]);
    if (!endsWith(waspFilepath, ".wasp")) { printHelp(); return 1; }

    // Read wasp file.
    std::string waspFileContent = readFile(waspFilepath);

    buildWebAppFromWasp(waspFileContent);

    std::cout << "Done! Created index.html." << std::endl;
}
