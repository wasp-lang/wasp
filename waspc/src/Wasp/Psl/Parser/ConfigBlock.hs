module Wasp.Psl.Parser.ConfigBlock
  ( configBlock,
  )
where

import Text.Parsec
  ( anyChar,
    char,
    many,
    manyTill,
    newline,
    optional,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Parser.Common
  ( braces,
    identifier,
    reserved,
    whiteSpace,
  )

configBlock :: Parser Psl.Schema.SchemaElement
configBlock = try datasource <|> try generator

-- | Parses a datasource.
-- Example of PSL datasource:
-- datasource db {
--   provider   = "postgresql"
--   url        = env("DATABASE_URL")
--   extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
-- }
datasource :: Parser Psl.Schema.SchemaElement
datasource = do
  whiteSpace
  reserved "datasource"
  datasourceName <- identifier
  content <- configBlockBody
  optional newline
  return $ Psl.Schema.SchemaDatasource $ Psl.ConfigBlock.Datasource datasourceName content

-- | Parses a generator.
-- Example of PSL generator:
-- generator client {
--   provider        = "prisma-client-js"
--   previewFeatures = ["postgresqlExtensions"]
-- }
generator :: Parser Psl.Schema.SchemaElement
generator = do
  whiteSpace
  reserved "generator"
  generatorName <- identifier
  content <- configBlockBody
  optional newline
  return $ Psl.Schema.SchemaGenerator $ Psl.ConfigBlock.Generator generatorName content

configBlockBody :: Parser [Psl.ConfigBlock.ConfigBlockKeyValue]
configBlockBody = braces (many keyValue)

-- | Parses a key-value pair.
-- Example of PSL key-value pair:
-- provider = "postgresql"
-- It works for both datasources and generators.
keyValue :: Parser Psl.ConfigBlock.ConfigBlockKeyValue
keyValue = do
  whiteSpace
  key <- identifier
  whiteSpace
  reserved "="
  whiteSpace
  value <- manyTill anyChar (char '\n')
  optional whiteSpace
  return $ Psl.ConfigBlock.ConfigBlockKeyValue key value
