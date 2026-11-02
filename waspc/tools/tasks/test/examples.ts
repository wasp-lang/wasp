#!/usr/bin/env node
//MISE description="Executes only examples e2e tests. Installs deps first if needed."
//MISE depends=["build-wasp-app-runner"]

import { execa } from "execa";
import { copyFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { styleText } from "node:util";
import { getRepoRootPath, getWaspcDirPath } from "../../utils.ts";

const repoRootDir = getRepoRootPath();
const waspCliDevPath = join(getWaspcDirPath(), "tools", "wasp-cli-dev");
const waspAppRunnerCliPath = join(
  repoRootDir,
  "packages/wasp-app-runner/bin/index.js",
);

const EXAMPLE_APP_PATHS = [
  "examples/tutorials/TodoApp",
  "examples/tutorials/TodoAppTs",
  "examples/waspello",
  "examples/waspleau",
  "examples/websockets-realtime-voting",
  "examples/ask-the-documents",
  "examples/kitchen-sink",
];

for (const exampleAppPath of EXAMPLE_APP_PATHS) {
  try {
    await testExampleApp(join(repoRootDir, exampleAppPath));
  } catch {
    const failureMessage = `E2E tests failed in ${exampleAppPath}`;
    console.error(styleText("red", failureMessage, { stream: process.stderr }));
    process.exit(1);
  }
}

async function testExampleApp(exampleAppDir: string): Promise<void> {
  const execaOptions = {
    cwd: exampleAppDir,
    env: {
      WASP_CLI_CMD: waspCliDevPath,
      WASP_APP_RUNNER_CLI_CMD: waspAppRunnerCliPath,
    },
    stdio: "inherit",
    verbose: "short",
  } as const;
  copyEnvServerExampleIfNeeded(exampleAppDir);
  await execa(waspCliDevPath, ["install"], execaOptions);
  await execa("npm", ["run", "test"], execaOptions);
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
