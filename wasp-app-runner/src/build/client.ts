import * as path from "path";
import * as fs from "fs";

import { spawnWithLog } from "../process.js";
import { createLogger } from "../logging.js";
import { EnvVars } from "../types.js";
import { parse } from "dotenv";
import { doesFileExits } from "../files.js";
import type { PathToApp } from "../args.js";

const clientAppDir = ".wasp/build/web-app";
const clientAppBuildOutputDir = path.join(clientAppDir, "build");

// Based on https://github.com/wasp-lang/wasp/issues/1883#issuecomment-2766265289
export async function buildAndRunClientApp({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): Promise<void> {
  const logger = createLogger("client-build-and-run-app");
  const { exitCode: buildExitCode } = await buildClientApp({ pathToApp });
  if (buildExitCode !== 0) {
    logger.error("Failed to build client app.");
    process.exit(1);
  }

  // This starts a long running process, so we don't await it.
  startClientApp({ pathToApp }).then(({ exitCode }) => {
    if (exitCode !== 0) {
      logger.error("Failed to start client app.");
      process.exit(1);
    }
  });
}

async function buildClientApp({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): Promise<{ exitCode: number | null }> {
  const defaultRequiredEnv = {
    REACT_APP_API_URL: "http://localhost:3001",
  };
  const devEnv = await getDevEnvVars({ pathToApp });

  const clientBuildEnv = {
    ...defaultRequiredEnv,
    ...devEnv,
  };

  return spawnWithLog({
    name: "client-build-app",
    cmd: "npm",
    args: ["run", "build"],
    cwd: path.join(pathToApp, clientAppDir),
    extraEnv: clientBuildEnv,
  });
}

async function startClientApp({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): Promise<{
  exitCode: number | null;
}> {
  return spawnWithLog({
    name: "client-start-app",
    cmd: "npx",
    args: ["serve", "--single", "-p", "3000"],
    cwd: path.join(pathToApp, clientAppBuildOutputDir),
  });
}

async function getDevEnvVars({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): Promise<EnvVars> {
  const envVarsPath = path.join(pathToApp, ".env.client");

  if (!doesFileExits(envVarsPath)) {
    return {};
  }
  const contents = fs.readFileSync(envVarsPath, "utf-8");
  return parse(contents);
}
