import { stripVTControlCharacters } from "node:util";
import semver, { type SemVer } from "semver";

import type { PathToApp, WaspCliCmd } from "./args.js";
import { DbType } from "./db/index.js";
import { createLogger } from "./logging.js";
import { Process } from "./process.js";
import type { Branded, EnvVars } from "./types.js";

export type AppName = Branded<string, "AppName">;
export type WaspVersion = Branded<SemVer, "WaspVersion">;

export function waspMigrateDb({
  waspCliCmd,
  pathToApp,
  extraEnv,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  extraEnv: EnvVars;
}): Promise<{ exitCode: number | null }> {
  return new Process({
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
    env: extraEnv,
  })
    .log("wasp-migrate-db")
    .wait();
}

export function waspStart({
  waspCliCmd,
  pathToApp,
  extraEnv,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  extraEnv: EnvVars;
}): Promise<{ exitCode: number | null }> {
  return new Process({
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "start"],
    cwd: pathToApp,
    env: extraEnv,
  })
    .log("wasp-start")
    .wait();
}

export function waspBuild({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<{ exitCode: number | null }> {
  return new Process({
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "build"],
    cwd: pathToApp,
  })
    .log("wasp-build")
    .wait();
}

export function waspBuildStart({
  waspCliCmd,
  pathToApp,
  serverEnvVars,
  clientEnvVars,
  serverEnvFile,
  clientEnvFile,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  serverEnvVars?: EnvVars;
  clientEnvVars?: EnvVars;
  serverEnvFile?: string;
  clientEnvFile?: string;
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

  return new Process({
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, ...args],
    cwd: pathToApp,
  })
    .log("wasp-build-start")
    .wait();
}

export async function getWaspVersion({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<{ waspVersion: WaspVersion }> {
  const logger = createLogger("wasp-version");
  const { stdout, exitCode } = await new Process({
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "version"],
    cwd: pathToApp,
  }).collect();
  const stdoutClean = stripVTControlCharacters(stdout);

  if (exitCode !== 0) {
    logger.fatal(`Failed to get wasp version: ${stdoutClean}`);
  }

  const [firstLine] = stdout.split("\n");
  const waspVersion = semver.parse(firstLine);

  if (!waspVersion) {
    logger.fatal("Failed to get wasp version");
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
  const logger = createLogger("wasp-info");
  const { stdout, exitCode } = await new Process({
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "info"],
    cwd: pathToApp,
  }).collect();
  const stdoutClean = stripVTControlCharacters(stdout);

  if (exitCode !== 0) {
    logger.fatal(`Failed to get app info: ${stdoutClean}`);
  }

  const appNameMatch = stdoutClean.match(/Name: (.*)$/m);
  const dbTypeMatch = stdoutClean.match(/Database system: (.*)$/m);

  if (appNameMatch === null) {
    logger.fatal("Failed to get app name");
  }

  if (dbTypeMatch === null) {
    logger.fatal("Failed to get database type");
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
  const { stderr, exitCode } = await new Process({
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "ts-setup"],
    cwd: pathToApp,
  }).collect();

  if (exitCode !== 0) {
    logger.fatal(`Failed to set up Wasp TypeScript config: ${stderr}`);
  }
}

function ensureRegexMatch(
  match: RegExpMatchArray | null,
  name: string,
): string {
  const logger = createLogger("ensure-regex-match");
  if (match === null) {
    return logger.fatal(`Failed to get ${name}`);
  }

  if (match.length !== 2) {
    return logger.fatal(`Got more than one ${name}`);
  }

  return match[1]!;
}
