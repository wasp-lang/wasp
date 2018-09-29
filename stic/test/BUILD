load(":macros.bzl", "simple_target_test")

# Catch2 is test framework that we are using. It has only one header file, which we included directly in code.
cc_library(
  name = "catch2",
  hdrs = ["catch.hpp"]
)

# In order to be able to run Catch2 tests, it is important to have this main method defined (as by Catch2 documentation).
cc_library(
    name = "catch-main",
    srcs = ["main.cpp"],
    deps = [
      ":catch2"
    ]
)

# Individual tests (per target).
# Each test expects a target in src/ and a .test.cpp file with same name to exist.
# Add new tests here! Also, add them to test suite all-tests below.
simple_target_test("utils")

# Test suite that runs all the tests.
test_suite(
    name = "all-tests",
    tests = [
        "utils"
    ]
)
