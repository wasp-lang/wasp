#include "catch.hpp"

#include "utils.hpp"

using namespace stic;

TEST_CASE("endsWith() returns true for suffix") {
    REQUIRE(utils::endsWith("ABCD", "CD") == true);
}

TEST_CASE("endsWith() returns false for non-suffix") {
    REQUIRE(utils::endsWith("ABCD", "C") == false);
}
