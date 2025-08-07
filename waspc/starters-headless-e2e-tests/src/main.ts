#!/usr/bin/env node

import { chalk } from "zx";
import { parseArgs } from "./cli.js";
import { runStarterHeadlessE2ETests } from "./starter-headless-e2e-test-runner.js";
import { STARTERS_HEADLESS_E2E_TESTS } from "./starter-headless-e2e-tests.js";

try {
  await runAllStartersE2ETests();
} catch (error) {
  console.error("Error: ", error);
  process.exit(1);
}

async function runAllStartersE2ETests(): Promise<void> {
  const { waspCliCommand } = parseArgs(process.argv);

  for (const starterHeadlessE2ETests of STARTERS_HEADLESS_E2E_TESTS) {
    console.log(
      chalk.bold(
        `\nRunning tests for ${starterHeadlessE2ETests.starterName} starter`,
      ),
    );
    await runStarterHeadlessE2ETests({
      waspCliCommand,
      starterHeadlessE2ETests,
    });
  }
}
