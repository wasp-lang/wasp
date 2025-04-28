import * as path from "path";
import * as fs from "fs";

import { spawnAndCollectOutput, spawnWithLog } from "../process.js";
import {
  createAppSpecificServerBuildDockerNames,
  ServerBuildContainerName,
  ServerBuildImageName,
} from "../docker.js";
import { EnvVars } from "../types.js";
import { log } from "../logging.js";

export async function buildAndRunServerAppContainer({
  appName,
  pathToApp,
  extraEnv,
}: {
  appName: string;
  pathToApp: string;
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

  log(
    "server-build-and-run-app",
    "success",
    `Built server app container: ${containerName}`
  );

  runServerAppContainer({
    pathToApp,
    imageName,
    containerName,
    extraEnv,
  });
}

function buildServerAppContainer({
  pathToApp,
  imageName,
}: {
  pathToApp: string;
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
  pathToApp: string;
  imageName: ServerBuildImageName;
  containerName: ServerBuildContainerName;
  extraEnv: EnvVars;
}): void {
  spawnWithLog({
    name: "server-start-app",
    cmd: "docker",
    args: [
      "run",
      "--rm",
      ...getServerAppEnvArgs({
        pathToApp,
        extraEnv,
      }),
      "--network",
      "host",
      "--name",
      containerName,
      imageName,
    ],
  }).then(({ exitCode }) => {
    if (exitCode !== 0) {
      process.exit(1);
    }
  });
}

function getServerAppEnvArgs({
  pathToApp,
  extraEnv,
}: {
  pathToApp: string;
  extraEnv: EnvVars;
}): string[] {
  const requiredEnv: EnvVars = {
    WASP_WEB_CLIENT_URL: "http://localhost:3000",
    JWT_SECRET: "some-jwt-secret",
    WASP_SERVER_URL: "http://localhost:3001",
  };
  const allEnv = { ...requiredEnv, ...extraEnv };
  return [
    ...envVarsToArgs(allEnv),
    ...getDevEnvVarsFileArg({
      pathToApp,
    }),
  ];
}

// Docker fails if the env file is not present, so we check if it exists first.
function getDevEnvVarsFileArg({
  pathToApp,
}: {
  pathToApp: string;
}): [string, string] | [] {
  const envFilePath = path.resolve(pathToApp, ".env.server");
  try {
    const stats = fs.statSync(envFilePath);
    if (stats.isFile()) {
      return ["--env-file", envFilePath];
    }
  } catch (err) {}

  return [];
}

function envVarsToArgs(envVars: EnvVars): string[] {
  return Object.entries(envVars).flatMap(([key, value]) => {
    return [`--env`, `${key}=${value}`];
  });
}
