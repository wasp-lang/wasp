#!/usr/bin/env node

import { chalk } from "zx";
import { parseArgs } from "./cli.js";
import { runStarterE2ETests } from "./starter-e2e-test-runner.js";
import { STARTER_NAMES } from "./starters.js";

try {
  await runAllStartersE2ETests();
} catch (error) {
  console.error("Error: ", error);
  process.exit(1);
}

async function runAllStartersE2ETests(): Promise<void> {
  const { waspCliCommand } = parseArgs(process.argv);

  for (const starterName of STARTER_NAMES) {
    console.log(chalk.bold(`\nRunning tests for ${starterName} starter`));
    await runStarterE2ETests({
      waspCliCommand,
      starterName,
    });
  }
}
