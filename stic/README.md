STIC
====

![stick](./stick.png)

## Setup

### Bazel
We are using Bazel as our build system, so make sure to install bazel globally on your machine.


## Building
We are using Bazel build system.
Everything is defined in BUILD files (+ one WORKSPACE file).
Learn more about Bazel to know how to build spepcific targets, locate produced binaries and so on.


## Running
For your convenience, there is `run-stick-cli` script that builds and runs `stick-cli` binary for you.

To get started, try running `./run-stick-cli demo/hello-world.wasp`.


## Tests
We are using Catch2 testing framework.

While you can use Bazel to run specific tests, there is `run-all-tests` script to easily build and run all tests.
