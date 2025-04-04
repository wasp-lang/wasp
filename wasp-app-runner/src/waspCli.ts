import { stripVTControlCharacters } from "node:util";

import { log } from "./logging.js";
import { spawnWithLog, spawnAndCollectOutput } from "./process.js";
import { DbType } from "./db/index.js";
import type { EnvVars } from "./types.js";

export function migrateDb({
  waspCliCmd,
  pathToApp,
  extraEnv,
}: {
  waspCliCmd: string;
  pathToApp: string;
  extraEnv: EnvVars;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
    name: "migrate-db",
    cmd: waspCliCmd,
    args: ["db", "migrate-dev"],
    cwd: pathToApp,
    extraEnv,
  });
}

export function startApp({
  waspCliCmd,
  pathToApp,
  extraEnv,
}: {
  waspCliCmd: string;
  pathToApp: string;
  extraEnv: EnvVars;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
    name: "start-app",
    cmd: waspCliCmd,
    args: ["start"],
    cwd: pathToApp,
    extraEnv,
  });
}

export async function getAppInfo({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: string;
  pathToApp: string;
}): Promise<{
  appName: string;
  dbType: DbType;
}> {
  const { stdoutData, exitCode } = await spawnAndCollectOutput({
    name: "get-app-info",
    cmd: waspCliCmd,
    args: ["info"],
    cwd: pathToApp,
  });
  const stdoutDataWithoutAnsiChars = stripVTControlCharacters(stdoutData);

  if (exitCode !== 0) {
    log(
      "get-app-info",
      "error",
      `Failed to get app info: ${stdoutDataWithoutAnsiChars}`
    );
    process.exit(1);
  }

  const appNameMatch = stdoutDataWithoutAnsiChars.match(/Name: (.*)$/m);
  const dbTypeMatch = stdoutDataWithoutAnsiChars.match(
    /Database system: (.*)$/m
  );

  if (appNameMatch === null) {
    log("get-app-info", "error", `Failed to get app name`);
    process.exit(1);
  }

  if (dbTypeMatch === null) {
    log("get-app-info", "error", `Failed to get database type`);
    process.exit(1);
  }

  return {
    appName: ensureRegexMatch(appNameMatch, "app name"),
    dbType:
      ensureRegexMatch(dbTypeMatch, "db type") === "PostgreSQL"
        ? DbType.Postgres
        : DbType.Sqlite,
  };
}

function ensureRegexMatch(
  match: RegExpMatchArray | null,
  name: string
): string {
  if (!match) {
    log("ensure-one-match", "error", `Failed to get ${name}`);
    process.exit(1);
  }

  if (match.length !== 2) {
    log("ensure-one-match", "error", `Got more than one ${name}`);
    process.exit(1);
  }

  return match[1]!;
}
