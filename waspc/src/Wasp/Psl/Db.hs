module Wasp.Psl.Db
  ( dbProviderPostgresqlStringLiteral,
    dbProviderSqliteStringLiteral,
  )
where

dbProviderPostgresqlStringLiteral :: String
dbProviderPostgresqlStringLiteral = "postgresql"

dbProviderSqliteStringLiteral :: String
dbProviderSqliteStringLiteral = "sqlite"
