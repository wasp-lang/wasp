#!/usr/bin/env node

// Entry point for the Wasp CLI to run the Wasp Spec analyzer of this
// project's `@wasp/spec`. The CLI finds the project's `@wasp/wasp` the way
// Node does and runs this file from there, so the import below resolves the
// `@wasp/spec` that `@wasp/wasp` pins, however the package manager laid out
// `node_modules`.
import "@wasp/spec/internal/run";
