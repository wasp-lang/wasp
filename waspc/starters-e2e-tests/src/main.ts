#!/usr/bin/env node

import { chalk } from "zx";
import { parseArgs } from "./cli.js";
import { runStarterE2ETests } from "./starter-e2e-test-runner.js";
import { STARTERS_E2E_TESTS } from "./starter-e2e-tests.js";

try {
  await runAllStartersE2ETests();
} catch (error) {
  console.error("Error: ", error);
  process.exit(1);
}

async function runAllStartersE2ETests(): Promise<void> {
  const { waspCliCommand } = parseArgs(process.argv);

  for (const starterE2ETests of STARTERS_E2E_TESTS) {
    console.log(
      chalk.bold(`\nRunning tests for ${starterE2ETests.starterName} starter`),
    );
    await runStarterE2ETests({
      waspCliCommand,
      starterE2ETests,
    });
  }
}
