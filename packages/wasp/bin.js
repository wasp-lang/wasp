#!/usr/bin/env node

// `@wasp/wasp` declares `@wasp/cli`'s `wasp` executable, because a Wasp app
// depends on `@wasp/wasp` directly, and every package manager links the
// executables of an app's direct dependencies. pnpm doesn't link them for
// `@wasp/cli`, which is only a dependency of this package.
import "@wasp/cli/bin";
