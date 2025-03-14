module Wasp.Psl.Db
  ( pslPostgresqlKeyword,
    pslSqliteKeyword,
  )
where

pslPostgresqlKeyword :: String
pslPostgresqlKeyword = "postgresql"

pslSqliteKeyword :: String
pslSqliteKeyword = "sqlite"
