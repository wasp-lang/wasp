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

-- | Parses a config block.
-- Example of PSL config block:
-- datasource db {
--   provider   = "postgresql"
--   url        = env("DATABASE_URL")
--   extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
-- }
configBlock :: Parser Psl.Schema.Block
configBlock = do
  whiteSpace
  configBlockType <-
    try (reserved "datasource" >> return Psl.ConfigBlock.Datasource)
      <|> try (reserved "generator" >> return Psl.ConfigBlock.Generator)
  name <- identifier
  content <- configBlockBody
  optional newline
  return $ Psl.Schema.ConfigBlock $ Psl.ConfigBlock.ConfigBlock configBlockType name content

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
