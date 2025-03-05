import { Options } from "../cli.js";
import { runAppWithPostgres } from "./postgres.js";
import { runAppWithSqlite } from "./sqlite.js";

type RunAppFn = (context: {
  extraEnv: Record<string, string>;
}) => Promise<void>;

export type RunAppWithDbFn = (
  options: Options,
  runApp: RunAppFn
) => Promise<void>;

export const dbTypes = ["sqlite", "postgres"] as const;
export type DbType = (typeof dbTypes)[number];

const dbTypeToRunAppFn: Record<DbType, RunAppWithDbFn> = {
  sqlite: runAppWithSqlite,
  postgres: runAppWithPostgres,
};

export function executeWithDb(options: Options, fn: RunAppFn): Promise<void> {
  return dbTypeToRunAppFn[options.dbType](options, fn);
}
