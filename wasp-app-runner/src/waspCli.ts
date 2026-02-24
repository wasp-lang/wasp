import { stripVTControlCharacters } from "node:util";
import semver, { type SemVer } from "semver";

import type { PathToApp, WaspCliCmd } from "./args.js";
import { DbType } from "./db/index.js";
import { createLogger } from "./logging.js";
import { spawnAndCollectOutput, spawnWithLog } from "./process.js";
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
  signal?: AbortSignal;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
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
  signal?: AbortSignal;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
    name: "wasp-start",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "start"],
    cwd: pathToApp,
    extraEnv,
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
  signal?: AbortSignal;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
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
  signal?: AbortSignal;
}): Promise<{ exitCode: number | null }> {
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

  return spawnWithLog({
    name: "wasp-build-start",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, ...args],
    cwd: pathToApp,
    signal,
  });
}

const waspInfoLogger = createLogger("wasp-info");

export async function getWaspVersion({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<{ waspVersion: WaspVersion }> {
  const { stdoutData, exitCode } = await spawnAndCollectOutput({
    name: "wasp-version",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "version"],
    cwd: pathToApp,
  });
  const stdoutDataWithoutAnsiChars = stripVTControlCharacters(stdoutData);

  if (exitCode !== 0) {
    throw waspInfoLogger.cliError(
      `Failed to get wasp version: ${stdoutDataWithoutAnsiChars}`,
    );
  }

  const [firstLine] = stdoutData.split("\n");
  const waspVersion = semver.parse(firstLine);

  if (!waspVersion) {
    throw waspInfoLogger.cliError("Failed to get wasp version");
  }

  return {
    waspVersion: waspVersion as WaspVersion,
  };
}

export async function waspInfo({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<{
  appName: AppName;
  dbType: DbType;
}> {
  const { stdoutData, exitCode } = await spawnAndCollectOutput({
    name: "wasp-info",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "info"],
    cwd: pathToApp,
  });
  const stdoutDataWithoutAnsiChars = stripVTControlCharacters(stdoutData);

  if (exitCode !== 0) {
    throw waspInfoLogger.cliError(
      `Failed to get app info: ${stdoutDataWithoutAnsiChars}`,
    );
  }

  const appNameMatch = stdoutDataWithoutAnsiChars.match(/Name: (.*)$/m);
  const dbTypeMatch = stdoutDataWithoutAnsiChars.match(
    /Database system: (.*)$/m,
  );

  if (appNameMatch === null) {
    throw waspInfoLogger.cliError("Failed to get app name");
  }

  if (dbTypeMatch === null) {
    throw waspInfoLogger.cliError("Failed to get database type");
  }

  return {
    appName: ensureRegexMatch(appNameMatch, "app name") as AppName,
    dbType:
      ensureRegexMatch(dbTypeMatch, "db type") === "PostgreSQL"
        ? DbType.Postgres
        : DbType.Sqlite,
  };
}

export async function waspTsSetup({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<void> {
  const logger = createLogger("wasp-ts-setup");
  const { stderrData, exitCode } = await spawnAndCollectOutput({
    name: "wasp-ts-setup",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "ts-setup"],
    cwd: pathToApp,
  });

  if (exitCode !== 0) {
    throw logger.cliError(
      `Failed to set up Wasp TypeScript config: ${stderrData}`,
    );
  }
}

function ensureRegexMatch(
  match: RegExpMatchArray | null,
  name: string,
): string {
  if (match === null) {
    throw waspInfoLogger.cliError(`Failed to get ${name}`);
  }

  if (match.length !== 2) {
    throw waspInfoLogger.cliError(`Got more than one ${name}`);
  }

  return match[1]!;
}
