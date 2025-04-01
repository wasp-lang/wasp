import { createHash } from "crypto";

import type { RunAppWithDbFn } from "./types.js";
import { log } from "../logging.js";
import { processManager } from "../process.js";
import { Branded } from "../types.js";

type ContainerName = Branded<string, "ContainerName">;
type DatabaseConnectionUrl = Branded<string, "DatabaseConnectionUrl">;

export const runAppWithPostgres: RunAppWithDbFn = async (
  { appName, pathToApp },
  runApp
) => {
  const databaseUrl = await ensurePostgresContainer({ appName, pathToApp });

  log("postgres", "info", `Using DATABASE_URL: ${databaseUrl}`);

  return runApp({
    extraEnv: { DATABASE_URL: databaseUrl },
  });
};

async function ensurePostgresContainer({
  appName,
  pathToApp,
}: {
  appName: string;
  pathToApp: string;
}): Promise<DatabaseConnectionUrl> {
  const containerName = createAppSpecificContainerName({
    appName,
    pathToApp,
  });

  try {
    log("postgres", "info", `Using DB container: ${containerName}`);

    const databaseUrl = await runPostgresContainerAndWaitUntilReady(
      containerName
    );

    return databaseUrl;
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
}): ContainerName {
  const appPathHash = createHash("md5")
    .update(pathToApp)
    .digest("hex")
    .slice(0, 16);
  return `${appName}-${appPathHash}-db` as ContainerName;
}

async function runPostgresContainerAndWaitUntilReady(
  containerName: ContainerName
): Promise<DatabaseConnectionUrl> {
  const port = 5432;
  const password = "devpass";
  const image = "postgres:16";

  log("postgres", "info", "Starting the PostgreSQL container...");

  processManager
    .spawnAndCollectStdout({
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
        `--rm`,
        image,
      ],
    })
    // If we awaited here, we would block the main thread indefinitely.
    .then(({ exitCode, stderrData }) => {
      if (exitCode !== 0) {
        log("postgres", "error", stderrData);
        log(
          "postgres",
          "error",
          `Maybe the cleanup failed, try running: "docker rm -f ${containerName}" and then try again.`
        );
        process.exit(1);
      }
    });

  await waitForPostgresReady(containerName);

  return `postgresql://postgres:${password}@localhost:${port}/postgres` as DatabaseConnectionUrl;
}

async function waitForPostgresReady(
  containerName: ContainerName
): Promise<void> {
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

async function checkIfPostgresIsReady(
  containerName: ContainerName
): Promise<boolean> {
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
