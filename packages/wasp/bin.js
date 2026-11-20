#!/usr/bin/env node

// pnpm only links the executables of direct dependencies, and a project depends
// on `@wasp/wasp`, not on `@wasp/cli`.
import "@wasp/cli/bin.js";
