module Wasp.Parser.Db
  ( db,
  )
where

import Data.Maybe (listToMaybe)
import Text.Parsec (try, (<|>))
import Text.Parsec.String (Parser)
import qualified Wasp.Lexer as L
import qualified Wasp.Parser.Common as P
import qualified Wasp.Wasp.Db as Wasp.Db

db :: Parser Wasp.Db.Db
db = do
  L.reserved L.reservedNameDb
  dbProperties <- P.waspClosure (L.commaSep1 dbProperty)

  system <-
    maybe
      (fail "'system' property is required!")
      return
      (listToMaybe [p | DbPropertySystem p <- dbProperties])

  return
    Wasp.Db.Db
      { Wasp.Db._system = system
      }

newtype DbProperty
  = DbPropertySystem Wasp.Db.DbSystem

dbProperty :: Parser DbProperty
dbProperty =
  dbPropertySystem

dbPropertySystem :: Parser DbProperty
dbPropertySystem = DbPropertySystem <$> P.waspProperty "system" dbPropertySystemValue
  where
    dbPropertySystemValue =
      try (L.symbol "PostgreSQL" >> return Wasp.Db.PostgreSQL)
        <|> try (L.symbol "SQLite" >> return Wasp.Db.SQLite)
