cc_library(
  name = "catch2",
  hdrs = ["catch.hpp"]
)

cc_library(
    name = "catch-main",
    srcs = ["main.cpp"],
    deps = [
      ":catch2"
    ]
)

# TODO: I could use macros to define each test individiually in easy way.

cc_test(
    name = "all-tests",
    srcs = glob(["**/*.test.cpp"]),
    copts = ["-Isrc/"],
    deps = [
        ":catch2",
        "catch-main",

        "//src:hello-world"
    ]
)
