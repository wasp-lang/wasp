import { createHash } from "crypto";

import type { RunAppWithDbFn } from "./types.js";
import { log } from "../logging.js";
import { processManager } from "../process.js";

export const runAppWithPostgres: RunAppWithDbFn = async (
  { appName, pathToApp },
  runApp
) => {
  const DATABASE_URL = await ensurePostgresContainer({ appName, pathToApp });

  log("postgres", "info", `Using DATABASE_URL: ${DATABASE_URL}`);

  return runApp({
    extraEnv: { DATABASE_URL },
  });
};

async function ensurePostgresContainer({
  appName,
  pathToApp,
}: {
  appName: string;
  pathToApp: string;
}): Promise<string> {
  const port = 5432;
  const password = "devpass";
  const image = "postgres:16";
  const containerName = createAppSpecificContainerName({
    appName,
    pathToApp,
  });

  try {
    log("postgres", "info", `Using DB container: ${containerName}`);

    const doesPostgresContainerExist = await checkIfPostgresContainerExists(
      containerName
    );

    if (doesPostgresContainerExist) {
      const isPostgresContainerRunning =
        await checkIfPostgresContainerIsRunning(containerName);

      if (isPostgresContainerRunning) {
        log("postgres", "info", "Stopping existing PostgreSQL container...");
        await stopPostgresContainer(containerName);
      }

      log("postgres", "info", "Deleting existing PostgreSQL container...");
      await deletePostgresContainer(containerName);
    }

    // We don't block on this, as we want to start the container in the background
    runPostgresContainer(containerName, port, password, image);

    await waitForPostgresReady(containerName);

    return `postgresql://postgres:${password}@localhost:${port}/postgres`;
  } catch (error: unknown) {
    if (error instanceof Error) {
      log("postgres", "error", error.message);
    } else {
      log("postgres", "error", `${error}`);
    }
    process.exit(1);
  }
}

function createAppSpecificContainerName({
  appName,
  pathToApp,
}: {
  appName: string;
  pathToApp: string;
}) {
  const appPathHash = createHash("md5")
    .update(pathToApp)
    .digest("hex")
    .slice(0, 16);
  return `${appName}-${appPathHash}-db`;
}

async function checkIfPostgresContainerExists(
  containerName: string
): Promise<boolean> {
  const { exitCode } = await processManager.spawnAndCollectStdout({
    name: "check-container-exists",
    cmd: "docker",
    args: ["inspect", "--format={{.Name}}", containerName],
  });

  return exitCode === 0;
}

async function checkIfPostgresContainerIsRunning(
  containerName: string
): Promise<boolean> {
  const { exitCode, stdoutData } = await processManager.spawnAndCollectStdout({
    name: "check-container-running",
    cmd: "docker",
    args: ["inspect", "--format={{.State.Running}}", containerName],
  });

  return exitCode === 0 && stdoutData.trim() === "true";
}

async function stopPostgresContainer(containerName: string): Promise<void> {
  const { exitCode } = await processManager.spawnAndCollectStdout({
    name: "stop-container",
    cmd: "docker",
    args: ["stop", containerName],
  });

  if (exitCode !== 0) {
    throw new Error("Failed to stop a PostgreSQL container");
  }
}

async function deletePostgresContainer(containerName: string): Promise<void> {
  const { exitCode } = await processManager.spawnAndCollectStdout({
    name: "delete-container",
    cmd: "docker",
    args: ["rm", containerName],
  });

  if (exitCode !== 0) {
    throw new Error("Failed to delete a PostgreSQL container");
  }
}

async function runPostgresContainer(
  containerName: string,
  port: number,
  password: string,
  image: string
): Promise<void> {
  const { exitCode } = await processManager.spawnAndCollectStdout({
    name: "create-postgres-container",
    cmd: "docker",
    args: [
      "run",
      "--name",
      containerName,
      "-p",
      `${port}:5432`,
      "-e",
      `POSTGRES_PASSWORD=${password}`,
      image,
    ],
  });

  if (exitCode !== 0) {
    throw new Error("Failed to run a PostgreSQL container");
  }
}

async function waitForPostgresReady(containerName: string): Promise<void> {
  const healthCheckRetries = 10;
  const healthCheckDelay = 2000;

  for (let i = 1; i <= healthCheckRetries; i++) {
    log(
      "postgres",
      "info",
      `Checking PostgreSQL readiness (attempt ${i}/${healthCheckRetries})`
    );

    const isPostgresReady = await checkIfPostgresIsReady(containerName);

    if (isPostgresReady) {
      log("postgres", "success", "PostgreSQL is ready");
      return;
    }
    await wait(healthCheckDelay);
  }

  throw new Error("PostgreSQL did not become ready in time");
}

async function checkIfPostgresIsReady(containerName: string): Promise<boolean> {
  const { exitCode } = await processManager.spawnAndCollectStdout({
    name: "postgres-readiness-check",
    cmd: "docker",
    args: ["exec", containerName, "pg_isready", "-U", "postgres"],
  });

  return exitCode === 0;
}

async function wait(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
