#!/usr/bin/env node

import { spawn } from "child_process";
import { existsSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parseArgs, type WaspCliCommand } from "./cli.js";
import {
  STARTER_TESTS,
  type StarterTemplateName,
  type StarterTests,
} from "./starter-tests.js";

const SCRIPT_DIRECTORY = dirname(fileURLToPath(import.meta.url));

try {
  await runAllStarterTests();
} catch (error) {
  console.error("Error: ", error);
  process.exit(1);
}

async function runAllStarterTests(): Promise<void> {
  const { waspCliCommand } = parseArgs(process.argv);

  for (const starterTests of STARTER_TESTS) {
    try {
      console.log(`Running tests for ${starterTests.templateName}...`);
      await runStarterTest({
        waspCliCommand,
        starterTests,
      });
      console.log(
        `Tests for ${starterTests.templateName} completed successfully.`,
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

async function runStarterTest(
  starterTestsExecution: StarterTestsExecution,
): Promise<void> {
  const starterTestsRunnerArgs = getStarterTestsRunnerArgs(
    starterTestsExecution,
  );

  const projectRootDirectory = findProjectRootDirectory(SCRIPT_DIRECTORY);
  const starterTestsRunnerPath = join(
    projectRootDirectory,
    "run-starter-e2e-tests.sh",
  );

  const childProcess = spawn(starterTestsRunnerPath, starterTestsRunnerArgs, {
    stdio: "inherit",
    shell: true,
  });

  return new Promise((resolve, reject) => {
    childProcess.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`Tests failed with exit code ${code}`));
      }
    });

    childProcess.on("error", (error) => {
      reject(error);
    });
  });
}

function findProjectRootDirectory(startingDirectory: string): string {
  let currentDirectory = startingDirectory;

  while (currentDirectory !== dirname(currentDirectory)) {
    if (existsSync(join(currentDirectory, "package.json"))) {
      return currentDirectory;
    }
    currentDirectory = dirname(currentDirectory);
  }
  
  throw new Error("Could not find project root directory");
}

type StarterTestsRunnerArgs =
  | [WaspCliCommand, StarterTemplateName, string, string]
  | [WaspCliCommand, StarterTemplateName, string];

function getStarterTestsRunnerArgs({
  waspCliCommand,
  starterTests: tests,
}: StarterTestsExecution): StarterTestsRunnerArgs {
  const starterTestsRunnerArgs: StarterTestsRunnerArgs = [
    waspCliCommand,
    tests.templateName,
    tests.appRelativePath,
  ];

  if ("remoteTestsRelativePath" in tests) {
    starterTestsRunnerArgs.push(tests.remoteTestsRelativePath);
  }

  return starterTestsRunnerArgs;
}



