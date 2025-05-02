import * as path from "path";

import { spawnWithLog } from "../process.js";
import {
  createAppSpecificServerBuildDockerNames,
  ServerBuildContainerName,
  ServerBuildImageName,
} from "../docker.js";
import { EnvVars } from "../types.js";
import { log } from "../logging.js";
import { doesFileExits } from "../files.js";
import type { AppName } from "../waspCli.js";
import type { PathToApp } from "../args.js";

export async function buildAndRunServerAppContainer({
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

  const { exitCode: buildExitCode } = await buildServerAppContainer({
    pathToApp,
    imageName,
  });

  if (buildExitCode !== 0) {
    log(
      "server-build-app",
      "error",
      `Failed to build server app container: ${containerName}`
    );
    process.exit(1);
  }

  // This starts a long running process, so we don't await it.
  runServerAppContainer({
    pathToApp,
    imageName,
    containerName,
    extraEnv,
  }).then(({ exitCode }) => {
    if (exitCode !== 0) {
      log(
        "server-start-app",
        "error",
        `Failed to start server app container: ${containerName}`
      );
      process.exit(1);
    }
  });
}

function buildServerAppContainer({
  pathToApp,
  imageName,
}: {
  pathToApp: PathToApp;
  imageName: ServerBuildImageName;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
    name: "server-build-app",
    cmd: "docker",
    args: ["build", "-t", imageName, "."],
    cwd: path.join(pathToApp, ".wasp/build"),
  });
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
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
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
    ...getEnvVarsDockerArgs({ ...defaultRequiredEnv, ...extraEnv }),
    ...getDevEnvFileDockerArg({
      pathToApp,
    }),
  ];
}

function getEnvVarsDockerArgs(envVars: EnvVars): string[] {
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
