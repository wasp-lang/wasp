#!/usr/bin/env node

import { spawn } from "child_process";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parseArgs, type WaspCliCommand } from "./cli.js";
import {
  RemoteTestsRelativePath,
  STARTER_TESTS,
  WaspProjectRelativePath,
  type StarterTemplateName,
  type StarterTests,
} from "./starter-tests.js";
import { findNodeProjectRootDirectory } from "./utils.js";


try {
  await runAllStartersE2ETests();
} catch (error) {
  console.error("Error: ", error);
  process.exit(1);
}

async function runAllStartersE2ETests(): Promise<void> {
  const { waspCliCommand } = parseArgs(process.argv);

  for (const starterTests of STARTER_TESTS) {
    try {
      console.log(`Running tests for ${starterTests.templateName}...`);
      await runStarterE2ETests({
        waspCliCommand,
        starterTests,
      });
      console.log(
        `Tests for ${starterTests.templateName} completed successfully!`,
      );
    } catch (error) {
      console.error(
        `Failed to run tests for ${starterTests.templateName}:`,
        error,
      );
      process.exit(1);
    }
  }
}

type StarterTestsExecution = {
  waspCliCommand: WaspCliCommand;
  starterTests: StarterTests;
};

async function runStarterE2ETests(
  starterTestsExecution: StarterTestsExecution,
): Promise<void> {
  const SCRIPT_DIRECTORY = dirname(fileURLToPath(import.meta.url));
  const STARTERS_E2E_TEST_RUNNER_SCRIPT_NAME = "run-starter-e2e-tests.sh";

  const starterTestsRunnerArgs = getStarterTestsRunnerArgs(
    starterTestsExecution,
  );

  const projectRootDirectory = findNodeProjectRootDirectory(SCRIPT_DIRECTORY);
  const starterTestsRunnerPath = join(
    projectRootDirectory,
    STARTERS_E2E_TEST_RUNNER_SCRIPT_NAME,
  );

  return new Promise((resolve, reject) => {
    const childProcess = spawn(starterTestsRunnerPath, starterTestsRunnerArgs, {
      stdio: "inherit",
      shell: true,
    });

    childProcess.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`Starter tests failed with exit code ${code}`));
      }
    });

    childProcess.on("error", (error) => {
      reject(error);
    });
  });
}

type StarterTestsRunnerArgs =
  | [
      WaspCliCommand,
      StarterTemplateName,
      WaspProjectRelativePath,
      RemoteTestsRelativePath,
    ]
  | [WaspCliCommand, StarterTemplateName, WaspProjectRelativePath];

function getStarterTestsRunnerArgs({
  waspCliCommand,
  starterTests,
}: StarterTestsExecution): StarterTestsRunnerArgs {
  const starterTestsRunnerArgs: StarterTestsRunnerArgs = [
    waspCliCommand,
    starterTests.templateName,
    starterTests.waspProjectRelativePath,
  ];

  if ("remoteTestsRelativePath" in starterTests) {
    starterTestsRunnerArgs.push(starterTests.includedTestsRelativePath);
  }

  return starterTestsRunnerArgs;
}
