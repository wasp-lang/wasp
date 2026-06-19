#!/usr/bin/env node
/// <reference types="node" />

// Runs the locally-built Wasp CLI through `cabal run`, so that dev and test
// invocations always reflect the current state of the `waspc` source without a
// prior `cabal install`.
//
// We expose this as a single executable (rather than a multi-word command
// string) so it can stand in wherever a plain `wasp` / `wasp-cli` binary is
// expected (e2e shell commands, wasp-app-runner, playwright configs) without
// each consumer having to special-case shell quoting and word splitting.

import { spawnSync } from "node:child_process";
import { getWaspcDirPath } from "./utils.ts";

const waspcDirPath = getWaspcDirPath();
const [_node, _filename, ...waspCliArgs] = process.argv;

const result = spawnSync(
  "cabal",
  [
    // "-v0" silences cabal's own logs so the CLI behaves like an installed binary.
    "-v0",
    `--project-dir=${waspcDirPath}`,
    "run",
    "wasp-cli",
    "--",
    ...waspCliArgs,
  ],
  { stdio: "inherit" },
);

if (result.error) {
  throw result.error;
}

process.exit(result.status ?? 1);
