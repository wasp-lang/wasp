#!/usr/bin/env node

// `@wasp/cli` owns the `wasp` executable. This package needs its own `bin`
// entry anyway, because package managers only link the executables of a
// project's direct dependencies (pnpm in particular), and `@wasp/cli` is a
// dependency of this package, not of the user's project.
import "@wasp/cli/bin.js";
