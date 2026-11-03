#!/usr/bin/env node

// `@wasp/cli` owns the `wasp` executable. pnpm only links the executables of
// a project's direct dependencies, and `@wasp/cli` is a dependency of this
// package, not of the project. So this package declares its own `wasp` bin,
// which runs `@wasp/cli`'s.
import "@wasp/cli/bin.js";
