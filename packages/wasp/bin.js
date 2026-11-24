#!/usr/bin/env node

// pnpm only links the executables of direct dependencies, and a project depends
// on `@wasp/wasp`, not on `@wasp/cli`. So we wrap the `@wasp/cli` executable
// here, instead of using it directly.
import "@wasp/cli/bin";
