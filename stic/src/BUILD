cc_library(
    name = "hello-world",
    srcs = ["hello-world.cpp"],
    hdrs = ["hello-world.hpp"],
    visibility = [
        "//test:__pkg__"
    ]
)

cc_binary(
    name = "stick-cli",
    srcs = ["cli.cpp"],
    deps = [
         ":hello-world"
    ]
)
