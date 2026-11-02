#!/usr/bin/env node
//MISE description="Executes all tests (waspc + Wasp CLI + Wasp LS + examples + starters). Builds the project first if needed."
//MISE depends=["workspace-install"]

import { execa } from "execa";
import { join } from "node:path";
import { getWaspcDirPath } from "../utils.ts";

const waspcDir = getWaspcDirPath();
const execaOptions = { stdio: "inherit", verbose: "short" } as const;

try {
  await execa("cabal", ["test"], {
    ...execaOptions,
    cwd: waspcDir,
    env: { WASP_CLI_CMD: join(waspcDir, "tools", "wasp-cli-dev") },
  });
  console.log("Running e2e tests");
  for (const task of [
    "test:starters",
    "test:examples",
    "test:libs",
    "test:packages",
  ]) {
    await execa("mise", ["run", task], execaOptions);
  }
  console.log("ALL TESTS PASSED");
} catch {
  console.log("SOME TESTS FAILED");
  process.exitCode = 1;
}
