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
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  extraEnv: EnvVars;
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
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "start"],
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
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "build"],
    cwd: pathToApp,
  });
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

  return spawnWithLog({
    name: "wasp-build-start",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, ...args],
    cwd: pathToApp,
  });
}

export async function getWaspVersion({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<{ waspVersion: WaspVersion }> {
  const logger = createLogger("wasp-info");
  const { stdoutData, exitCode } = await spawnAndCollectOutput({
    name: "wasp-version",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "version"],
    cwd: pathToApp,
  });
  const stdoutDataWithoutAnsiChars = stripVTControlCharacters(stdoutData);

  if (exitCode !== 0) {
    logger.error(`Failed to get wasp version: ${stdoutDataWithoutAnsiChars}`);
    process.exit(1);
  }

  const [firstLine] = stdoutData.split("\n");
  const waspVersion = semver.parse(firstLine);

  if (!waspVersion) {
    logger.error("Failed to get wasp version");
    process.exit(1);
  }

  return {
    waspVersion: waspVersion as WaspVersion,
  };
}

export type AppInfo = {
  appName: AppName;
  dbType: DbType;
};

/**
 * Wasp 0.26.0 replaced `wasp info` with `wasp show spec`, so we try the new
 * command first and fall back to the old one for older Wasp versions.
 */
export async function waspInfo({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<AppInfo> {
  const appInfoFromShowSpec = await getAppInfoFromShowSpec({
    waspCliCmd,
    pathToApp,
  });

  return appInfoFromShowSpec ?? getAppInfoFromInfo({ waspCliCmd, pathToApp });
}

/**
 * Returns `null` if `wasp show spec` isn't available (i.e. the app runs an
 * older Wasp version), so the caller can fall back to `wasp info`.
 */
async function getAppInfoFromShowSpec({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<AppInfo | null> {
  const logger = createLogger("wasp-show-spec");
  const { stdoutData, stderrData, exitCode } = await spawnAndCollectOutput({
    name: "wasp-show-spec",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "show", "spec", "--json"],
    cwd: pathToApp,
  });

  if (exitCode !== 0) {
    logger.debug(
      `Falling back to "wasp info", "wasp show spec" failed: ${stripVTControlCharacters(stderrData)}`,
    );
    return null;
  }

  let spec: unknown;
  try {
    spec = JSON.parse(stripVTControlCharacters(stdoutData));
  } catch (error) {
    logger.error(`Failed to parse the app spec: ${error}`);
    process.exit(1);
  }

  if (!isRecord(spec)) {
    logger.error("Failed to get the app spec");
    process.exit(1);
  }

  const appDecl = (Array.isArray(spec.decls) ? spec.decls : []).find(
    (decl: unknown) => isRecord(decl) && decl.declType === "App",
  );

  if (!isRecord(appDecl) || typeof appDecl.declName !== "string") {
    logger.error("Failed to get app name");
    process.exit(1);
  }

  if (typeof spec.dbSystem !== "string") {
    logger.error("Failed to get database type");
    process.exit(1);
  }

  return {
    appName: appDecl.declName as AppName,
    dbType: spec.dbSystem === "PostgreSQL" ? DbType.Postgres : DbType.Sqlite,
  };
}

async function getAppInfoFromInfo({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<AppInfo> {
  const logger = createLogger("wasp-info");
  const { stdoutData, exitCode } = await spawnAndCollectOutput({
    name: "wasp-info",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "info"],
    cwd: pathToApp,
  });
  const stdoutDataWithoutAnsiChars = stripVTControlCharacters(stdoutData);

  if (exitCode !== 0) {
    logger.error(`Failed to get app info: ${stdoutDataWithoutAnsiChars}`);
    process.exit(1);
  }

  const appNameMatch = stdoutDataWithoutAnsiChars.match(/Name: (.*)$/m);
  const dbTypeMatch = stdoutDataWithoutAnsiChars.match(
    /Database system: (.*)$/m,
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

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

export async function waspInstall({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<void> {
  const logger = createLogger("wasp-install");
  const { stderrData, exitCode } = await spawnAndCollectOutput({
    name: "wasp-install",
    cmd: waspCliCmd.cmd,
    args: [...waspCliCmd.args, "install"],
    cwd: pathToApp,
  });

  if (exitCode !== 0) {
    logger.error(`Failed to install Wasp project dependencies: ${stderrData}`);
    process.exit(1);
  }
}

function ensureRegexMatch(
  match: RegExpMatchArray | null,
  name: string,
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
