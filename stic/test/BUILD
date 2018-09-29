cc_library(
    name = "catch-main",
    srcs = ["main.cpp"],
    copts = ["-Ivendor/catch2/"],
    deps = [
      "//vendor/catch2:catch2"
    ]
)

cc_test(
    name = "all-tests",
    srcs = glob(["**/*.test.cpp"]),
    copts = ["-Ivendor/catch2/", "-Isrc/"],
    deps = [
      "catch-main",
      "//vendor/catch2:catch2",
      "//src:hello-world"
    ]
)
