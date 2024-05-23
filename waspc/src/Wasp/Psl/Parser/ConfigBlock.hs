module Wasp.Psl.Parser.ConfigBlock
  ( configBlock,
  )
where

import Text.Parsec
  ( many1,
    newline,
    noneOf,
    optional,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Model as Model
import Wasp.Psl.Parser.Common
  ( braces,
    identifier,
    reserved,
    whiteSpace,
  )

configBlock :: Parser Model.SchemaElement
configBlock = try datasource <|> try generator

-- | Parses a datasource.
-- Example of PSL datasource:
-- datasource db {
--   provider   = "postgresql"
--   url        = env("DATABASE_URL")
--   extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
-- }
datasource :: Parser Model.SchemaElement
datasource = do
  whiteSpace
  _ <- reserved "datasource"
  datasourceName <- identifier
  content <- braces (many1 keyValue)
  return $ Model.SchemaDatasource $ Model.Datasource datasourceName content

-- | Parses a generator.
-- Example of PSL generator:
-- generator client {
--   provider        = "prisma-client-js"
--   previewFeatures = ["postgresqlExtensions"]
-- }
generator :: Parser Model.SchemaElement
generator = do
  whiteSpace
  _ <- reserved "generator"
  generatorName <- identifier
  content <- braces (many1 keyValue)
  return $ Model.SchemaGenerator $ Model.Generator generatorName content

-- | Parses a key-value pair.
-- Example of PSL key-value pair:
-- provider = "postgresql"
-- It works for both datasources and generators.
keyValue :: Parser Model.ConfigBlockKeyValue
keyValue = do
  whiteSpace
  key <- identifier
  whiteSpace
  _ <- reserved "="
  whiteSpace
  value <- many1 (noneOf "\n") -- value can be anything until
  optional newline
  return $ Model.ConfigBlockKeyValue key value
