import * as path from "path";

import type { PathToApp } from "../args.js";
import {
  createAppSpecificServerBuildDockerNames,
  ServerBuildContainerName,
  ServerBuildImageName,
} from "../docker.js";
import { doesFileExits } from "../files.js";
import { createLogger } from "../logging.js";
import { spawnWithLog } from "../process.js";
import { EnvVars } from "../types.js";
import type { AppName } from "../waspCli.js";

const serverAppDir = ".wasp/build";

// Based on https://github.com/wasp-lang/wasp/issues/1883#issuecomment-2766265289
export async function buildAndRunServerApp({
  appName,
  pathToApp,
  extraEnv,
}: {
  appName: AppName;
  pathToApp: PathToApp;
  extraEnv: EnvVars;
}): Promise<void> {
  const { imageName, containerName } = createAppSpecificServerBuildDockerNames({
    appName,
    pathToApp,
  });

  await buildServerAppContainer({
    pathToApp,
    imageName,
  });

  // This starts a long running process, so we don't await it.
  runServerAppContainer({
    pathToApp,
    imageName,
    containerName,
    extraEnv,
  });
}

export async function buildServerApp({
  appName,
  pathToApp,
}: {
  appName: AppName;
  pathToApp: PathToApp;
}): Promise<void> {
  const { imageName } = createAppSpecificServerBuildDockerNames({
    appName,
    pathToApp,
  });

  await buildServerAppContainer({
    pathToApp,
    imageName,
  });
}

export function startServerApp({
  appName,
  pathToApp,
  extraEnv,
}: {
  appName: AppName;
  pathToApp: PathToApp;
  extraEnv: EnvVars;
}): void {
  const { imageName, containerName } = createAppSpecificServerBuildDockerNames({
    appName,
    pathToApp,
  });

  // This starts a long running process, so we don't await it.
  runServerAppContainer({
    pathToApp,
    imageName,
    containerName,
    extraEnv,
  });
}

async function buildServerAppContainer({
  pathToApp,
  imageName,
}: {
  pathToApp: PathToApp;
  imageName: ServerBuildImageName;
}): Promise<void> {
  const logger = createLogger("server-build-app");
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
}

function runServerAppContainer({
  pathToApp,
  imageName,
  containerName,
  extraEnv,
}: {
  pathToApp: PathToApp;
  imageName: ServerBuildImageName;
  containerName: ServerBuildContainerName;
  extraEnv: EnvVars;
}): void {
  const logger = createLogger("server-start-app");
  spawnWithLog({
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
  }).catch((error) => {
    logger.error(`Failed to start server app container: ${containerName} with exit code: ${error.exitCode ?? 'unknown'}`);
    process.exit(1);
  });
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
