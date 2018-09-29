#include "catch.hpp"

#include "hello-world.hpp"

TEST_CASE("Returns hello world.") {
    REQUIRE(sayHi() == "Hello world!");
}
