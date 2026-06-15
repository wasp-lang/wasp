import { stripVTControlCharacters } from "node:util";
import semver, { type SemVer } from "semver";

import type { PathToApp, WaspCliCmd } from "./args.js";
import { DbType } from "./db/index.js";
import {
  captureCommand,
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
  // `wasp start` spawns npm -> vite/node grandchildren, so it needs its own
  // process group to be killed as a tree.
  return spawnProcess({
    name: "wasp-start",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "start"],
    cwd: pathToApp,
    extraEnv,
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

  // Like `wasp start`, the build server spawns grandchildren; run it detached.
  return spawnProcess({
    name: "wasp-build-start",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, ...args],
    cwd: pathToApp,
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
  const { stdout } = await captureCommand({
    name: "wasp-version",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "version"],
    cwd: pathToApp,
    signal,
  });

  const [firstLine] = stdout.split("\n");
  const waspVersion = semver.parse(firstLine);

  if (!waspVersion) {
    throw new Error("Failed to parse wasp version");
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
  const { stdout } = await captureCommand({
    name: "wasp-info",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "info"],
    cwd: pathToApp,
    signal,
  });
  const output = stripVTControlCharacters(stdout);

  const appNameMatch = output.match(/Name: (.*)$/m);
  const dbTypeMatch = output.match(/Database system: (.*)$/m);

  return {
    appName: ensureRegexMatch(appNameMatch, "app name") as AppName,
    dbType:
      ensureRegexMatch(dbTypeMatch, "db type") === "PostgreSQL"
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
  await captureCommand({
    name: "wasp-install",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "install"],
    cwd: pathToApp,
    signal,
  });
}

function ensureRegexMatch(
  match: RegExpMatchArray | null,
  name: string,
): string {
  if (match === null) {
    throw new Error(`Failed to get ${name}`);
  }

  if (match.length !== 2) {
    throw new Error(`Got more than one ${name}`);
  }

  return match[1]!;
}
