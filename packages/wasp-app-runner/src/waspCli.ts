import { stripVTControlCharacters } from "node:util";

import type { PathToApp, WaspCliCmd } from "./args.js";
import { DbType } from "./db/index.js";
import { createLogger } from "./logging.js";
import { spawnAndCollectOutput, spawnWithLog } from "./process.js";
import type { Branded, EnvVars } from "./types.js";

export type AppName = Branded<string, "AppName">;

export function waspMigrateDb({
  waspCliCmd,
  pathToApp,
  extraEnv,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  extraEnv: EnvVars;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
    name: "wasp-migrate-db",
    cmd: waspCliCmd,
    /**
     * We use the --name flag because sometimes we run apps without a migrations directory,
     * which causes Prisma to prompt for a migration name interactively. This would make
     * the runner wait for input indefinitely.
     * Prisma timestamps all migration filenames automatically.
     * See: https://github.com/wasp-lang/runner-action/issues/7
     */
    args: ["db", "migrate-dev", "--name", "auto-migration"],
    cwd: pathToApp,
    extraEnv,
  });
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
  return spawnWithLog({
    name: "wasp-start",
    cmd: waspCliCmd,
    args: ["start"],
    cwd: pathToApp,
    extraEnv,
  });
}

export function waspBuild({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
    name: "wasp-build",
    cmd: waspCliCmd,
    args: ["build"],
    cwd: pathToApp,
  });
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
  const { stdoutData, exitCode } = await spawnAndCollectOutput({
    name: "wasp-info",
    cmd: waspCliCmd,
    args: ["info"],
    cwd: pathToApp,
  });
  const stdoutDataWithoutAnsiChars = stripVTControlCharacters(stdoutData);

  if (exitCode !== 0) {
    logger.error(`Failed to get app info: ${stdoutDataWithoutAnsiChars}`);
    process.exit(1);
  }

  const appNameMatch = stdoutDataWithoutAnsiChars.match(/Name: (.*)$/m);
  const dbTypeMatch = stdoutDataWithoutAnsiChars.match(
    /Database system: (.*)$/m
  );

  if (appNameMatch === null) {
    logger.error("Failed to get app name");
    process.exit(1);
  }

  if (dbTypeMatch === null) {
    logger.error("Failed to get database type");
    process.exit(1);
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
    cmd: waspCliCmd,
    args: ["ts-setup"],
    cwd: pathToApp,
  });

  if (exitCode !== 0) {
    logger.error(`Failed to set up Wasp TypeScript config: ${stderrData}`);
    process.exit(1);
  }
}

function ensureRegexMatch(
  match: RegExpMatchArray | null,
  name: string
): string {
  const logger = createLogger("ensure-regex-match");
  if (match === null) {
    logger.error(`Failed to get ${name}`);
    process.exit(1);
  }

  if (match.length !== 2) {
    logger.error(`Got more than one ${name}`);
    process.exit(1);
  }

  return match[1]!;
}
