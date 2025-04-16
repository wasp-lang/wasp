import { setupPostgres } from "./postgres.js";
import { setupSqlite } from "./sqlite.js";
import { setupLibsql } from "./libsql.js";
import type { SetupDbFn } from "./types.js";

export enum DbType {
  Sqlite = "sqlite",
  Postgres = "postgres",
  Libsql = "libsql",
}

export function setupDb({
  appName,
  dbType,
  pathToApp,
}: {
  dbType: DbType;
  appName: string;
  pathToApp: string;
}): ReturnType<SetupDbFn> {
  switch (dbType) {
    case DbType.Sqlite:
      return setupSqlite({ appName, pathToApp });
    case DbType.Postgres:
      return setupPostgres({ appName, pathToApp });
    case DbType.Libsql:
      return setupLibsql({ appName, pathToApp });
    default:
      dbType satisfies never;
      throw new Error(`Unknown database type: ${dbType}`);
  }
}
