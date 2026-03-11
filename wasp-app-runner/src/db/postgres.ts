import { setTimeout as delay } from "node:timers/promises";
import type { DockerImageName, PathToApp } from "../args.js";
import {
  DbContainerName,
  createAppSpecificDbContainerName,
} from "../docker.js";
import { createLogger } from "../logging.js";
import { Process } from "../process.js";
import { startServer } from "../server.js";
import { Branded } from "../types.js";
import * as asyncIterable from "../util/async-iterable.js";
import type { AppName } from "../waspCli.js";
import type { SetupDbResult } from "./types.js";

export const defaultPostgresDbImage = "postgres:18" as DockerImageName;

type DatabaseConnectionUrl = Branded<string, "DatabaseConnectionUrl">;

const logger = createLogger("postgres");

export const setupPostgres = async ({
  appName,
  pathToApp,
  dbImage,
}: {
  appName: AppName;
  pathToApp: PathToApp;
  dbImage: DockerImageName;
}): Promise<SetupDbResult> => {
  await ensureDockerIsRunning();

  const postgres = await startPostgresContainerForApp({
    appName,
    pathToApp,
    dbImage,
  });

  logger.info(`Using DATABASE_URL: ${postgres.databaseUrl}`);

  return {
    [Symbol.dispose]: postgres[Symbol.dispose],
    dbEnvVars: { DATABASE_URL: postgres.databaseUrl },
  };
};

async function startPostgresContainerForApp({
  appName,
  pathToApp,
  dbImage,
}: {
  appName: AppName;
  pathToApp: PathToApp;
  dbImage: DockerImageName;
}): Promise<
  Disposable & {
    databaseUrl: DatabaseConnectionUrl;
  }
> {
  const containerName = createAppSpecificDbContainerName({
    appName,
    pathToApp,
  });

  logger.info(`Using container name: ${containerName}`);

  const port = 5432;
  const password = "devpass";

  logger.info(`Starting the PostgreSQL container with image: ${dbImage}...`);

  const processLogger = createLogger("postgres-container");

  const postgres = await startServer(
    processLogger,
    {
      cmd: "docker",
      args: [
        "run",
        "--name",
        containerName,
        "-p",
        `${port}:5432`,
        "-e",
        `POSTGRES_PASSWORD=${password}`,
        `--rm`,
        dbImage,
      ],
    },
    async (proc) =>
      Promise.race([
        waitForPostgresReady(containerName),
        asyncIterable.forEach(proc.stderrLines, (line) => {
          const extraInfo = getExtraInfoOnPostgresStartError({
            originalErrorText: line,
            containerName,
            port,
          });
          if (extraInfo !== null) {
            processLogger.fatal(extraInfo);
          }
        }),
      ]),
  );

  return {
    [Symbol.dispose]: postgres[Symbol.dispose],
    databaseUrl:
      `postgresql://postgres:${password}@localhost:${port}/postgres` as DatabaseConnectionUrl,
  };
}

function getExtraInfoOnPostgresStartError({
  originalErrorText,
  containerName,
  port,
}: {
  originalErrorText: string;
  containerName: DbContainerName;
  port: number;
}): string | null {
  const errorText = originalErrorText.toLowerCase();

  if (errorText.includes("is already in use by container")) {
    return `It looks like the cleanup failed, try running: "docker rm -f ${containerName}" and then try again.`;
  }

  if (errorText.includes("port is already allocated")) {
    return `It seems the port ${port} is already in use. Stop any other process using this port and try again.`;
  }

  return null;
}

async function waitForPostgresReady(
  containerName: DbContainerName,
): Promise<void> {
  const healthCheckRetries = 10;
  const healthCheckDelay = 2000;

  for (let i = 1; i <= healthCheckRetries; i++) {
    logger.info(
      `Checking PostgreSQL readiness (attempt ${i}/${healthCheckRetries})`,
    );

    const isPostgresReady = await checkIfPostgresIsReady(containerName);

    if (isPostgresReady) {
      logger.success("PostgreSQL is ready");
      return;
    }

    await delay(healthCheckDelay);
  }

  logger.fatal("PostgreSQL did not become ready in time");
}

async function checkIfPostgresIsReady(
  containerName: DbContainerName,
): Promise<boolean> {
  return await new Process({
    cmd: "docker",
    args: ["exec", containerName, "pg_isready", "-U", "postgres"],
  })
    .wait()
    .then(({ exitCode }) => exitCode === 0)
    .catch(() => false);
}

async function ensureDockerIsRunning(): Promise<void> {
  const isDockerRunning = await checkIfDockerIsRunning();

  if (isDockerRunning) {
    return;
  }

  logger.fatal("Docker is not running. Please start Docker and try again.");
}

async function checkIfDockerIsRunning(): Promise<boolean> {
  return await new Process({
    cmd: "docker",
    args: ["info"],
  })
    .wait()
    .then(({ exitCode }) => exitCode === 0)
    .catch(() => false);
}
