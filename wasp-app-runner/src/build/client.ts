import * as fs from "fs";
import * as path from "path";

import { parse } from "dotenv";
import type { PathToApp } from "../args.js";
import { doesFileExits } from "../files.js";
import { waitUntilAppReady } from "../http.js";
import { createLogger } from "../logging.js";
import { spawnWithLog } from "../process.js";
import { EnvVars } from "../types.js";

const clientAppDir = ".wasp/build/web-app";

export async function buildClientApp({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): Promise<void> {
  const logger = createLogger("client-build-app");

  const defaultRequiredEnv = {
    REACT_APP_API_URL: "http://localhost:3001",
  };
  const devEnv = await getDevEnvVars({ pathToApp });

  const clientBuildEnv = {
    ...defaultRequiredEnv,
    ...devEnv,
  };

  const { exitCode } = await spawnWithLog({
    name: "client-build-app",
    cmd: "npm",
    args: ["run", "build"],
    cwd: path.join(pathToApp, clientAppDir),
    extraEnv: clientBuildEnv,
  });

  if (exitCode !== 0) {
    logger.error("Failed to build client app.");
    process.exit(1);
  }
}

/**
 * This function returns a Promise that resolves when the client app has started,
 * or rejects if it fails to start.
 *
 * The returned Promise in the object resolves when the client app process exits.
 */
export async function startClientApp({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): Promise<{ processPromise: Promise<{ exitCode: number | null }> }> {
  const logger = createLogger("client-start-app");

  const clientAppProcess = spawnWithLog({
    name: "client-start-app",
    cmd: "npm",
    args: [
      "run",
      "preview",
      "--",
      "--port",
      "3000",
      "--strictPort", // This will make it fail if the port is already in use.
    ],
    cwd: path.join(pathToApp, clientAppDir),
  });

  const clientAppExitCodePromise = clientAppProcess.then(({ exitCode }) => {
    if (exitCode !== 0) {
      logger.error("Failed to start client app.");
      process.exit(1);
    }
  });

  await Promise.race([waitUntilAppReady({ port: 3000 }), clientAppExitCodePromise]);

  return { processPromise: clientAppProcess };
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
