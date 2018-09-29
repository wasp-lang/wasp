cc_library(
    name = "utils",
    srcs = ["utils.cpp"],
    hdrs = ["utils.hpp"],
    visibility = [
        "//test:__pkg__"
    ]
)

cc_binary(
    name = "stick-cli",
    srcs = ["cli.cpp"],
    deps = [
         ":utils"
    ]
)
