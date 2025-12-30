import * as path from "path";

import type { PathToApp } from "../args.js";
import {
  createAppSpecificServerBuildDockerNames,
  ServerBuildContainerName,
  ServerBuildImageName,
} from "../docker.js";
import { doesFileExits } from "../files.js";
import { waitUntilHTTP } from "../http.js";
import { createLogger } from "../logging.js";
import { spawnWithLog } from "../process.js";
import { EnvVars } from "../types.js";
import type { AppName } from "../waspCli.js";

const serverAppDir = ".wasp/build";

export async function buildServerAppContainer({
  appName,
  pathToApp,
}: {
  appName: AppName;
  pathToApp: PathToApp;
}): Promise<{
  imageName: ServerBuildImageName;
  containerName: ServerBuildContainerName;
}> {
  const logger = createLogger("server-build-app");

  const { imageName, containerName } = createAppSpecificServerBuildDockerNames({
    appName,
    pathToApp,
  });

  const { exitCode } = await spawnWithLog({
    name: "server-build-app",
    cmd: "docker",
    args: ["build", "-t", imageName, "."],
    cwd: path.join(pathToApp, serverAppDir),
  });

  if (exitCode !== 0) {
    logger.error(`Failed to build server app image: ${imageName}`);
    process.exit(1);
  }

  return { imageName, containerName };
}

/**
 * This function returns a Promise that resolves when the server container app
 * has started, or rejects if it fails to start.
 *
 * The returned Promise in the object resolves when the server container app
 * process exits.
 */
export async function runServerAppContainer({
  pathToApp,
  imageName,
  containerName,
  extraEnv,
}: {
  pathToApp: PathToApp;
  imageName: ServerBuildImageName;
  containerName: ServerBuildContainerName;
  extraEnv: EnvVars;
}): Promise<{ processPromise: Promise<{ exitCode: number | null }> }> {
  const logger = createLogger("server-start-app");

  const childProcess = spawnWithLog({
    name: "server-start-app",
    cmd: "docker",
    args: [
      "run",
      "--rm",
      ...getDockerEnvVarsArgs({
        pathToApp,
        extraEnv,
      }),
      "--network",
      "host",
      "--name",
      containerName,
      imageName,
    ],
  });

  const exitCodePromise = childProcess.then(({ exitCode }) => {
    if (exitCode !== 0) {
      logger.error(`Failed to start server app container: ${containerName}`);
      process.exit(1);
    }
  });

  await Promise.race([waitUntilHTTP({ port: 3001 }), exitCodePromise]);

  return { processPromise: childProcess };
}

function getDockerEnvVarsArgs({
  pathToApp,
  extraEnv,
}: {
  pathToApp: PathToApp;
  extraEnv: EnvVars;
}): string[] {
  const defaultRequiredEnv: EnvVars = {
    WASP_WEB_CLIENT_URL: "http://localhost:3000",
    JWT_SECRET: "some-jwt-secret",
    WASP_SERVER_URL: "http://localhost:3001",
  };
  return [
    ...mapEnvVarsToDockerArgs({ ...defaultRequiredEnv, ...extraEnv }),
    ...getDevEnvFileDockerArg({
      pathToApp,
    }),
  ];
}

function mapEnvVarsToDockerArgs(envVars: EnvVars): string[] {
  return Object.entries(envVars).flatMap(([key, value]) => {
    return [`--env`, `${key}=${value}`];
  });
}

function getDevEnvFileDockerArg({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): [string, string] | [] {
  const envFilePath = path.resolve(pathToApp, ".env.server");

  // Docker run command will fail if the file does not exist, so we check for it here.
  if (!doesFileExits(envFilePath)) {
    return [];
  }

  return ["--env-file", envFilePath];
}
