#!/usr/bin/env node

// Entry point for the Wasp CLI to run the Wasp Spec analyzer of this
// project's `@wasp/spec`. The CLI runs this file by path from the project's
// `node_modules/@wasp/wasp`, so Node resolves `@wasp/spec` next to this
// package no matter how the package manager laid out `node_modules`.
import "@wasp/spec/internal/run";
