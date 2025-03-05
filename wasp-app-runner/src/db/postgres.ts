import type { RunAppWithDbFn } from "./index.js";

import { log } from "../logging.js";
import { processManager } from "../process.js";
import { wait } from "../promises.js";
import { Options } from "../cli.js";

export const runAppWithPostgres: RunAppWithDbFn = async (options, runApp) => {
  const DATABASE_URL = await ensurePostgresContainer(options);

  log("postgres", "info", `Using DATABASE_URL: ${DATABASE_URL}`);

  await runApp({
    extraEnv: { DATABASE_URL },
  });
};

function getPostgresConfig(options: Options) {
  return {
    port: 5432,
    password: "devpass",
    image: "postgres:16",
    healthCheckRetries: 10,
    healthCheckDelay: 2000,
    containerName: `${options.appName}-postgres`,
  };
}

async function ensurePostgresContainer(options: Options) {
  const { containerName, port, password, image } = getPostgresConfig(options);
  const DATABASE_URL = `postgresql://postgres:${password}@localhost:${port}/postgres`;

  try {
    log("postgres", "info", `Using DB container: ${containerName}`);

    const exists = await postgresContainerExists(containerName);

    if (exists) {
      const isRunning = await postgresContainerIsRunning(containerName);

      if (isRunning) {
        log("postgres", "info", "Stopping existing PostgreSQL container...");
        await stopPostgresContainer(containerName);
      }

      log("postgres", "info", "Deleting existing PostgreSQL container...");
      await deletePostgresContainer(containerName);
    }

    // We don't block on this, as we want to start the container in the background
    runPostgresContainer(containerName, port, password, image).catch(
      (error) => {
        throw error;
      }
    );

    await waitForPostgresReady(options);

    return DATABASE_URL;
  } catch (error: unknown) {
    if (error instanceof Error) {
      log("postgres", "error", error.message);
    } else {
      log("postgres", "error", `${error}`);
    }
    process.exit(1);
  }
}

async function postgresContainerExists(
  containerName: string
): Promise<boolean> {
  const { exitCode } = await processManager.spawnAndCollectStdout({
    name: "check-container-exists",
    cmd: "docker",
    args: ["inspect", "--format={{.Name}}", containerName],
  });

  return exitCode === 0;
}

async function postgresContainerIsRunning(
  containerName: string
): Promise<boolean> {
  const { exitCode, stdoutData } = await processManager.spawnAndCollectStdout({
    name: "check-container-running",
    cmd: "docker",
    args: ["inspect", "--format={{.State.Running}}", containerName],
  });

  return exitCode === 0 && stdoutData.trim() === "true";
}

async function stopPostgresContainer(containerName: string): Promise<boolean> {
  const { exitCode } = await processManager.spawnAndCollectStdout({
    name: "stop-container",
    cmd: "docker",
    args: ["stop", containerName],
  });

  return exitCode === 0;
}

async function deletePostgresContainer(
  containerName: string
): Promise<boolean> {
  const { exitCode } = await processManager.spawnAndCollectStdout({
    name: "delete-container",
    cmd: "docker",
    args: ["rm", containerName],
  });

  return exitCode === 0;
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

async function waitForPostgresReady(options: Options) {
  const { containerName, healthCheckRetries, healthCheckDelay } =
    getPostgresConfig(options);

  for (let i = 1; i <= healthCheckRetries; i++) {
    log(
      "postgres",
      "info",
      `Checking PostgreSQL readiness (attempt ${i}/${healthCheckRetries})`
    );

    const { exitCode } = await processManager.spawnAndCollectStdout({
      name: "postgres-readiness-check",
      cmd: "docker",
      args: ["exec", containerName, "pg_isready", "-U", "postgres"],
    });

    if (exitCode === 0) {
      log("postgres", "success", "PostgreSQL is ready");

      return;
    }
    await wait(healthCheckDelay);
  }

  throw new Error("PostgreSQL did not become ready in time");
}
