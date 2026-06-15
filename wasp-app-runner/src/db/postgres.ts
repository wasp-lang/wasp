import pRetry from "p-retry";
import type { DockerImageName, PathToApp } from "../args.js";
import {
  ContainerName,
  createAppSpecificContainerName,
  pullDockerImage,
  startContainer,
  type ContainerHandle,
} from "../docker.js";
import { createLogger } from "../logging.js";
import { commandSucceeds } from "../process.js";
import { Branded } from "../types.js";
import type { AppName } from "../waspCli.js";
import type { SetupDbResult } from "./types.js";

export const defaultPostgresDbImage = "postgres:18" as DockerImageName;

const POSTGRES_PORT = 5432;
const POSTGRES_PASSWORD = "devpass";
const READINESS_RETRIES = 10;

type DatabaseConnectionUrl = Branded<string, "DatabaseConnectionUrl">;

const logger = createLogger("postgres");

export const setupPostgres = async ({
  appName,
  pathToApp,
  dbImage,
  signal,
}: {
  appName: AppName;
  pathToApp: PathToApp;
  dbImage: DockerImageName;
  signal: AbortSignal;
}): Promise<SetupDbResult> => {
  await pullDockerImage(dbImage, { signal });

  const containerName = createAppSpecificContainerName("db", {
    appName,
    pathToApp,
  });
  logger.info(`Using container name: ${containerName}`);
  logger.info(`Starting the PostgreSQL container with image: ${dbImage}...`);

  // Own the container in a stack so that if readiness fails or is aborted, the
  // container is disposed (removed) before the error propagates. On success we
  // `move()` ownership into the returned SetupDbResult.
  await using stack = new AsyncDisposableStack();
  const container = stack.use(
    await startContainer({
      name: "postgres",
      containerName,
      image: dbImage,
      dockerRunArgs: [
        "-p",
        `${POSTGRES_PORT}:5432`,
        "-e",
        `POSTGRES_PASSWORD=${POSTGRES_PASSWORD}`,
      ],
      output: "collect",
      signal,
    }),
  );

  await waitForPostgresReady({ container, containerName, signal });

  const databaseUrl =
    `postgresql://postgres:${POSTGRES_PASSWORD}@localhost:${POSTGRES_PORT}/postgres` as DatabaseConnectionUrl;
  logger.info(`Using DATABASE_URL: ${databaseUrl}`);

  const ownedStack = stack.move();
  return {
    name: container.name,
    exited: container.exited,
    dbEnvVars: { DATABASE_URL: databaseUrl },
    async [Symbol.asyncDispose]() {
      await ownedStack.disposeAsync();
    },
  };
};

async function waitForPostgresReady({
  container,
  containerName,
  signal,
}: {
  container: ContainerHandle;
  containerName: ContainerName;
  signal: AbortSignal;
}): Promise<void> {
  // Local controller lets us cancel the pending p-retry timers as soon as the
  // race settles, so they can't hold the event loop open.
  const readinessCtl = new AbortController();
  const readinessSignal = AbortSignal.any([signal, readinessCtl.signal]);

  try {
    const result = await Promise.race([
      pRetry(
        async (attempt) => {
          logger.info(
            `Checking PostgreSQL readiness (attempt ${attempt}/${READINESS_RETRIES + 1})`,
          );
          const isReady = await commandSucceeds({
            name: "postgres-readiness-check",
            cmd: "docker",
            args: ["exec", containerName, "pg_isready", "-U", "postgres"],
            signal: readinessSignal,
          });
          if (!isReady) {
            throw new Error("PostgreSQL is not ready yet");
          }
        },
        {
          retries: READINESS_RETRIES,
          factor: 1,
          minTimeout: 2000,
          signal: readinessSignal,
        },
      ).then(() => "ready" as const),
      container.exited.then((exit) => ({ exit }) as const),
    ]);

    if (result === "ready") {
      logger.success("PostgreSQL is ready");
      return;
    }

    // The container died before becoming ready.
    const stderr = container.stderrSoFar();
    const extraInfo = getExtraInfoOnPostgresStartError({
      originalErrorText: stderr,
      containerName,
      port: POSTGRES_PORT,
    });
    throw new Error(
      `The PostgreSQL container exited before becoming ready.\n${stderr}${
        extraInfo === null ? "" : `\n${extraInfo}`
      }`,
    );
  } finally {
    readinessCtl.abort();
  }
}

function getExtraInfoOnPostgresStartError({
  originalErrorText,
  containerName,
  port,
}: {
  originalErrorText: string;
  containerName: ContainerName;
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
