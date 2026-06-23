import type { DockerImageName, PathToApp } from "../args.ts";
import type { AppName } from "../waspCli.ts";
import { setupPostgres } from "./postgres.ts";
import { setupSqlite } from "./sqlite.ts";
import type { SetupDbResult } from "./types.ts";

export const DbType = {
  Sqlite: "sqlite",
  Postgres: "postgres",
} as const;
export type DbType = (typeof DbType)[keyof typeof DbType];

export function setupDb({
  appName,
  dbType,
  pathToApp,
  dbImage,
}: {
  dbType: DbType;
  appName: AppName;
  pathToApp: PathToApp;
  dbImage: DockerImageName;
}): Promise<SetupDbResult> {
  switch (dbType) {
    case DbType.Sqlite:
      return setupSqlite();
    case DbType.Postgres:
      return setupPostgres({ appName, pathToApp, dbImage });
    default:
      dbType satisfies never;
      throw new Error(`Unknown database type: ${dbType}`);
  }
}
