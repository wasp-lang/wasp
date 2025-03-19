import { stripVTControlCharacters } from "node:util";

import { log } from "./logging.js";
import { getWaspcDirAbsPath } from "./path.js";
import { processManager } from "./process.js";
import { DbType } from "./db/index.js";

export function migrateDb({
  waspCliCmd,
  pathToApp,
  extraEnv,
}: {
  waspCliCmd: string;
  pathToApp: string;
  extraEnv: Record<string, string>;
}): Promise<number | null> {
  return processManager.spawnWithLog({
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
  extraEnv: Record<string, string>;
}): Promise<number | null> {
  return processManager.spawnWithLog({
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
  const { stdoutData, exitCode } = await processManager.spawnAndCollectStdout({
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

  if (!appNameMatch) {
    log("get-app-info", "error", `Failed to get app name`);
    process.exit(1);
  }

  if (!dbTypeMatch) {
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

export async function installWaspCli(): Promise<number | null> {
  log("install-wasp-cli", "info", "Installing Wasp CLI globally...");

  return processManager.spawnWithLog({
    name: "install-wasp-cli",
    cmd: "cabal",
    args: ["install", "--overwrite-policy=always"],
    cwd: getWaspcDirAbsPath(),
  });
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
