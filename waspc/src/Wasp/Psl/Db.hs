module Wasp.Psl.Db
  ( dbProviderPostgresqlStringLiteral,
    dbProviderSqliteStringLiteral,
  )
where

dbProviderPostgresqlStringLiteral :: String
dbProviderPostgresqlStringLiteral = "postgresql"

dbProviderSqliteStringLiteral :: String
dbProviderSqliteStringLiteral = "sqlite"

dbProviderLibsqlStringLiteral :: String
-- While it's called libSQL, Prisma calls it "sqlite". libSQL is a fork of SQLite.
dbProviderLibsqlStringLiteral = "sqlite"
