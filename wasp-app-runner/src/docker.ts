import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import type { DockerImageName, PathToApp } from "./args.js";
import { registerExitGuard } from "./exitGuard.js";
import { createLogger } from "./logging.js";
import {
  CommandError,
  captureCommand,
  commandSucceeds,
  runCommand,
  spawnProcess,
  type OutputMode,
  type ProcessExit,
} from "./process.js";
import type { ManagedService } from "./run.js";
import { Branded } from "./types.js";
import type { AppName } from "./waspCli.js";

export type ContainerName = Branded<string, "ContainerName">;

export type ContainerKind = "db" | "smtp";

export interface ContainerHandle extends ManagedService {
  /** A running container's client always has an exit (never null). */
  readonly exited: Promise<ProcessExit>;
  /** The container client's stderr so far, for composing failure hints. */
  stderrSoFar(): string;
}

const logger = createLogger("docker");
const pullLogger = createLogger("docker-pull");

export async function ensureDockerIsRunning({
  signal,
}: {
  signal: AbortSignal;
}): Promise<void> {
  const isRunning = await commandSucceeds({
    name: "docker-health-check",
    cmd: "docker",
    args: ["info"],
    signal,
  });

  if (!isRunning) {
    throw new Error(
      "Docker is not running. Please start Docker and try again.",
    );
  }
}

export async function pullDockerImage(
  image: DockerImageName,
  { signal }: { signal: AbortSignal },
): Promise<void> {
  pullLogger.info(`Pulling Docker image: ${image}...`);
  try {
    await runCommand({
      name: "docker-pull",
      cmd: "docker",
      args: ["pull", image],
      signal,
    });
  } catch (error) {
    throw new Error(`Failed to pull Docker image: ${image}`, { cause: error });
  }
}

/**
 * Daemon-side, force removal of a container (with its anonymous volumes).
 *
 * Deliberately uses its OWN timeout instead of the run signal: it must work
 * even while we're shutting down because the run signal was aborted.
 */
export async function removeContainerIfExists(
  containerName: ContainerName,
): Promise<void> {
  try {
    await captureCommand({
      name: "docker-rm",
      cmd: "docker",
      args: ["rm", "--force", "--volumes", containerName],
      signal: AbortSignal.timeout(15_000),
    });
  } catch (error) {
    if (
      error instanceof CommandError &&
      (error.stderr ?? "").toLowerCase().includes("no such container")
    ) {
      return; // already gone
    }
    throw error;
  }
}

/**
 * Starts a container via `docker run --rm` and returns a managed handle.
 *
 * Disposal removes the container daemon-side (surviving our own death and
 * sidestepping postgres' "smart shutdown" hang) and then terminates the
 * attached client. No pre-flight removal of a same-named container: a name
 * collision surfaces through the caller's existing hint.
 */
export async function startContainer({
  name,
  containerName,
  image,
  dockerRunArgs,
  output,
  signal,
}: {
  name: string;
  containerName: ContainerName;
  image: DockerImageName;
  dockerRunArgs: string[];
  output: OutputMode;
  signal: AbortSignal;
}): Promise<ContainerHandle> {
  await ensureDockerIsRunning({ signal });

  // Attached (not detached): Playwright's group-SIGTERM reaches the client
  // directly and `--rm` cleans up on a graceful stop, in parallel with us.
  const client = spawnProcess({
    name,
    cmd: "docker",
    args: ["run", "--name", containerName, "--rm", ...dockerRunArgs, image],
    detached: false,
    signal,
    output,
  });

  // Synchronous last-resort removal if we die without async teardown.
  const unregisterGuard = registerExitGuard(() => {
    spawnSync("docker", ["rm", "--force", "--volumes", containerName], {
      stdio: "ignore",
      timeout: 5000,
    });
  });

  let disposed = false;
  return {
    name,
    exited: client.exited,
    stderrSoFar: () => client.collectedOutput.stderr,
    async [Symbol.asyncDispose]() {
      if (disposed) {
        return;
      }
      disposed = true;
      try {
        await removeContainerIfExists(containerName);
        await client.terminate();
      } catch (error) {
        logger.warn(
          `Failed to clean up container ${containerName}: ${String(error)}`,
        );
      } finally {
        unregisterGuard();
      }
    },
  };
}

export function createAppSpecificContainerName(
  kind: ContainerKind,
  {
    appName,
    pathToApp,
  }: {
    appName: AppName;
    pathToApp: PathToApp;
  },
): ContainerName {
  const prefix = createAppSpecificPrefix({ appName, pathToApp });
  return `${prefix}-${kind}` as ContainerName;
}

function createAppSpecificPrefix({
  appName,
  pathToApp,
}: {
  appName: AppName;
  pathToApp: PathToApp;
}): string {
  const appPathHash = createHash("md5")
    .update(pathToApp)
    .digest("hex")
    .slice(0, 16);
  return `${appName}-${appPathHash}`.toLowerCase();
}
