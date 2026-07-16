#!/bin/sh
# Stand-in for the real `wasp` CLI used only in tests: any invocation
# (notably `wasp build`) fails fast, so tests can prove that behavior
# upstream of the build (e.g. warn-not-block preflight) ran, without ever
# running a real Wasp build.
echo "stub wasp CLI: always fails" >&2
exit 1
