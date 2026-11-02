/// <reference types="node" />

import { copyFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { styleText } from "node:util";
import type { Command } from "../command.ts";
import { repoRootDir, runCmd, runInRepoRoot, waspcDir } from "../runCmd.ts";
import { runWaspCli, waspCliDevEnv } from "../waspCliDev.ts";
import { installWorkspace } from "../workspace.ts";

// Wasp App Runner
// Utility used for running e2e tests in Wasp apps, we use it for testing
// e.g. examples and starters.
// By passing this, we are telling all of our e2e tests to use the local
// version of Wasp App Runner, built by `buildWaspAppRunner` (tests should
// make sure to call that one before).
const waspAppRunnerCliPath = join(
  repoRootDir,
  "packages/wasp-app-runner/bin/index.js",
);
const e2eTestsEnv = {
  ...waspCliDevEnv,
  WASP_APP_RUNNER_CLI_CMD: waspAppRunnerCliPath,
};

const KITCHEN_SINK_PATH = "examples/kitchen-sink";
const EXAMPLE_APP_PATHS = [
  "examples/tutorials/TodoApp",
  "examples/tutorials/TodoAppTs",
  "examples/waspello",
  "examples/waspleau",
  "examples/websockets-realtime-voting",
  "examples/ask-the-documents",
  KITCHEN_SINK_PATH,
];

export const appE2eTestCommands: Command[] = [
  {
    name: "test:kitchen-sink",
    description:
      "Executes only kitchen-sink e2e tests. Installs deps first if needed.",
    run: async () => {
      await buildWaspAppRunner();
      await testExampleApp(KITCHEN_SINK_PATH);
    },
  },
  {
    name: "test:examples",
    description:
      "Executes only examples e2e tests. Installs deps first if needed.",
    run: testExampleApps,
  },
  {
    name: "test:starters",
    description:
      "Executes only starter templates e2e tests. Installs deps first if needed.",
    run: testStarters,
  },
];

export async function testExampleApps(): Promise<void> {
  await buildWaspAppRunner();
  for (const exampleAppPath of EXAMPLE_APP_PATHS) {
    try {
      await testExampleApp(exampleAppPath);
    } catch (error) {
      const failureMessage = `E2E tests failed in ${exampleAppPath}`;
      console.error(
        styleText("red", failureMessage, { stream: process.stderr }),
      );
      throw error;
    }
  }
}

export async function testStarters(): Promise<void> {
  await buildWaspAppRunner();
  await runCmd("pnpm", ["run", "test:dev"], {
    cwd: join(waspcDir, "starters-e2e-tests"),
    env: e2eTestsEnv,
  });
}

async function buildWaspAppRunner(): Promise<void> {
  await installWorkspace();
  await runInRepoRoot("pnpm", [
    "--filter",
    "@wasp/wasp-app-runner",
    "run",
    "build",
  ]);
}

async function testExampleApp(exampleAppPath: string): Promise<void> {
  const exampleAppDir = join(repoRootDir, exampleAppPath);
  copyEnvServerExampleIfNeeded(exampleAppDir);
  await runWaspCli(["install"], exampleAppDir);
  await runCmd("npm", ["run", "test"], {
    cwd: exampleAppDir,
    env: e2eTestsEnv,
  });
}

// Copies .env.server.example to .env.server in the app directory,
// but only if the example exists and .env.server isn't already present.
function copyEnvServerExampleIfNeeded(appDir: string): void {
  const envServerExamplePath = join(appDir, ".env.server.example");
  const envServerPath = join(appDir, ".env.server");
  if (existsSync(envServerExamplePath) && !existsSync(envServerPath)) {
    copyFileSync(envServerExamplePath, envServerPath);
  }
}
