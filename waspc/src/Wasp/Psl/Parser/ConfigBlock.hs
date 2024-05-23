module Wasp.Psl.Parser.ConfigBlock
  ( configBlock,
  )
where

import Text.Parsec
  ( many,
    many1,
    newline,
    noneOf,
    optional,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
import Wasp.Psl.Parser.Common
  ( braces,
    identifier,
    reserved,
    whiteSpace,
  )

configBlock :: Parser Psl.Ast.SchemaElement
configBlock = try datasource <|> try generator

-- | Parses a datasource.
-- Example of PSL datasource:
-- datasource db {
--   provider   = "postgresql"
--   url        = env("DATABASE_URL")
--   extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
-- }
datasource :: Parser Psl.Ast.SchemaElement
datasource = do
  whiteSpace
  reserved "datasource"
  datasourceName <- identifier
  content <- configBlockBody
  optional newline
  return $ Psl.Ast.SchemaDatasource $ Psl.Ast.Datasource datasourceName content

-- | Parses a generator.
-- Example of PSL generator:
-- generator client {
--   provider        = "prisma-client-js"
--   previewFeatures = ["postgresqlExtensions"]
-- }
generator :: Parser Psl.Ast.SchemaElement
generator = do
  whiteSpace
  reserved "generator"
  generatorName <- identifier
  content <- configBlockBody
  optional newline
  return $ Psl.Ast.SchemaGenerator $ Psl.Ast.Generator generatorName content

configBlockBody :: Parser [Psl.Ast.ConfigBlockKeyValue]
configBlockBody = braces (many keyValue)

-- | Parses a key-value pair.
-- Example of PSL key-value pair:
-- provider = "postgresql"
-- It works for both datasources and generators.
keyValue :: Parser Psl.Ast.ConfigBlockKeyValue
keyValue = do
  whiteSpace
  key <- identifier
  whiteSpace
  reserved "="
  whiteSpace
  value <- many1 (noneOf "\n") -- value can be anything until
  optional newline
  return $ Psl.Ast.ConfigBlockKeyValue key value
