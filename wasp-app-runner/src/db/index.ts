import { runAppWithPostgres } from "./postgres.js";
import { runAppWithSqlite } from "./sqlite.js";
import { RunAppFn } from "./types.js";

export enum DbType {
  Sqlite = "sqlite",
  Postgres = "postgres",
}
export const dbTypes = Object.values(DbType);

export function executeWithDb(
  {
    appName,
    dbType,
    pathToApp,
  }: { dbType: DbType; appName: string; pathToApp: string },
  fn: RunAppFn
): Promise<void> {
  switch (dbType) {
    case DbType.Sqlite:
      return runAppWithSqlite({ appName, pathToApp }, fn);
    case DbType.Postgres:
      return runAppWithPostgres({ appName, pathToApp }, fn);
    default:
      dbType satisfies never;
      throw new Error(`Unknown database type: ${dbType}`);
  }
}
