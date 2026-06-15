import { stripVTControlCharacters } from "node:util";
import semver, { type SemVer } from "semver";

import type { PathToApp, WaspCliCmd } from "./args.js";
import { DbType } from "./db/index.js";
import { createLogger, type Logger } from "./logging.js";
import {
  captureCommand,
  CommandError,
  runCommand,
  spawnProcess,
  type ProcessHandle,
} from "./process.js";
import type { Branded, EnvVars } from "./types.js";

export type AppName = Branded<string, "AppName">;
export type WaspVersion = Branded<SemVer, "WaspVersion">;

export function waspMigrateDb({
  waspCliCmd,
  pathToApp,
  extraEnv,
  signal,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  extraEnv: EnvVars;
  signal: AbortSignal;
}): Promise<void> {
  return runCommand({
    name: "wasp-migrate-db",
    cmd: waspCliCmd.cmd,
    /**
     * We use the --name flag because sometimes we run apps without a migrations directory,
     * which causes Prisma to prompt for a migration name interactively. This would make
     * the runner wait for input indefinitely.
     * Prisma timestamps all migration filenames automatically.
     * See: https://github.com/wasp-lang/runner-action/issues/7
     */
    args: [...waspCliCmd.args, "db", "migrate-dev", "--name", "auto-migration"],
    cwd: pathToApp,
    extraEnv,
    signal,
  });
}

export function waspStart({
  waspCliCmd,
  pathToApp,
  extraEnv,
  signal,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  extraEnv: EnvVars;
  signal: AbortSignal;
}): ProcessHandle {
  return spawnProcess({
    name: "wasp-start",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "start"],
    cwd: pathToApp,
    extraEnv,
    // `wasp start` spawns its own processes, so need to be set `detached` to
    // get its own process group and be killed as a tree.
    detached: true,
    signal,
  });
}

export function waspBuild({
  waspCliCmd,
  pathToApp,
  signal,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  signal: AbortSignal;
}): Promise<void> {
  return runCommand({
    name: "wasp-build",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "build"],
    cwd: pathToApp,
    signal,
  });
}

export function waspBuildStart({
  waspCliCmd,
  pathToApp,
  serverEnvVars,
  clientEnvVars,
  serverEnvFile,
  clientEnvFile,
  signal,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  serverEnvVars?: EnvVars;
  clientEnvVars?: EnvVars;
  serverEnvFile?: string;
  clientEnvFile?: string;
  signal: AbortSignal;
}): ProcessHandle {
  const args = [
    "build",
    "start",
    ...(serverEnvVars
      ? Object.entries(serverEnvVars).flatMap(([key, value]) => [
          "--server-env",
          `${key}=${value}`,
        ])
      : []),
    ...(clientEnvVars
      ? Object.entries(clientEnvVars).flatMap(([key, value]) => [
          "--client-env",
          `${key}=${value}`,
        ])
      : []),
    ...(serverEnvFile ? ["--server-env-file", serverEnvFile] : []),
    ...(clientEnvFile ? ["--client-env-file", clientEnvFile] : []),
  ];

  return spawnProcess({
    name: "wasp-build-start",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, ...args],
    cwd: pathToApp,
    // `wasp build start` spawns its own processes, so need to be set `detached`
    // to get its own process group and be killed as a tree.
    detached: true,
    signal,
  });
}

export async function getWaspVersion({
  waspCliCmd,
  pathToApp,
  signal,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  signal?: AbortSignal;
}): Promise<{ waspVersion: WaspVersion }> {
  const logger = createLogger("wasp-version");
  const stdout = await captureWaspCommand({
    logger,
    failureSummary: "Failed to get wasp version",
    cmd: waspCliCmd,
    extraArgs: ["version"],
    pathToApp,
    signal,
  });

  const [firstLine] = stdout.split("\n");
  const waspVersion = semver.parse(firstLine);

  if (!waspVersion) {
    logger.fatal(`Failed to parse wasp version from: ${firstLine}`);
  }

  return {
    waspVersion: waspVersion as WaspVersion,
  };
}

export async function waspInfo({
  waspCliCmd,
  pathToApp,
  signal,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  signal?: AbortSignal;
}): Promise<{
  appName: AppName;
  dbType: DbType;
}> {
  const logger = createLogger("wasp-info");
  const stdout = await captureWaspCommand({
    logger,
    failureSummary: "Failed to get app info",
    cmd: waspCliCmd,
    extraArgs: ["info"],
    pathToApp,
    signal,
  });
  const output = stripVTControlCharacters(stdout);

  const appNameMatch = output.match(/Name: (.*)$/m);
  const dbTypeMatch = output.match(/Database system: (.*)$/m);

  return {
    appName: ensureRegexMatch(logger, appNameMatch, "app name") as AppName,
    dbType:
      ensureRegexMatch(logger, dbTypeMatch, "db type") === "PostgreSQL"
        ? DbType.Postgres
        : DbType.Sqlite,
  };
}

export async function waspInstall({
  waspCliCmd,
  pathToApp,
  signal,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  signal?: AbortSignal;
}): Promise<void> {
  const logger = createLogger("wasp-install");
  await captureWaspCommand({
    logger,
    failureSummary: "Failed to install Wasp project dependencies",
    cmd: waspCliCmd,
    extraArgs: ["install"],
    pathToApp,
    signal,
  });
}

/**
 * Runs a `wasp` subcommand and returns its stdout. On failure it raises a fatal
 * error that keeps the original command output as context.
 */
async function captureWaspCommand({
  logger,
  failureSummary,
  cmd,
  extraArgs,
  pathToApp,
  signal,
}: {
  logger: Logger;
  failureSummary: string;
  cmd: WaspCliCmd;
  extraArgs: string[];
  pathToApp: PathToApp;
  signal?: AbortSignal;
}): Promise<string> {
  try {
    const { stdout } = await captureCommand({
      name: extraArgs[0] ? `wasp-${extraArgs[0]}` : "wasp",
      cmd: cmd.cmd,
      args: [...cmd.args, ...extraArgs],
      cwd: pathToApp,
      signal,
    });
    return stdout;
  } catch (error) {
    if (error instanceof CommandError) {
      const details = (error.stderr ?? "").trim();
      logger.fatal(`${failureSummary}${details ? `:\n${details}` : ""}`, {
        cause: error,
      });
    }
    throw error;
  }
}

function ensureRegexMatch(
  logger: Logger,
  match: RegExpMatchArray | null,
  name: string,
): string {
  if (match === null) {
    logger.fatal(`Failed to get ${name}`);
  }

  if (match.length !== 2) {
    logger.fatal(`Got more than one ${name}`);
  }

  return match[1]!;
}
