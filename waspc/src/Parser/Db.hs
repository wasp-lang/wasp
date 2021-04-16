module Parser.Db
  ( db
  ) where

import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), try)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Wasp.Db
import qualified Parser.Common as P
import qualified Lexer as L

db :: Parser Wasp.Db.Db
db = do
    L.reserved L.reservedNameDb
    dbProperties <- P.waspClosure (L.commaSep1 dbProperty)

    system <- fromMaybe (fail "'system' property is required!") $ return <$>
              listToMaybe [p | DbPropertySystem p <- dbProperties]

    return Wasp.Db.Db
        { Wasp.Db._system = system
        }

data DbProperty
    = DbPropertySystem Wasp.Db.DbSystem

dbProperty :: Parser DbProperty
dbProperty
    = dbPropertySystem

dbPropertySystem :: Parser DbProperty
dbPropertySystem = DbPropertySystem <$> (P.waspProperty "system" dbPropertySystemValue)
  where
    dbPropertySystemValue = try (L.symbol "PostgreSQL" >> return Wasp.Db.PostgreSQL)
                            <|> try (L.symbol "SQLite" >> return Wasp.Db.SQLite)
