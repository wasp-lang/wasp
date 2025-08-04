{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Wasp.Psl.Parser.ConfigBlock
  ( configBlock,
  )
where

import Text.Megaparsec (choice, many, try)
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import Wasp.Psl.Parser.Argument (expression)
import Wasp.Psl.Parser.Common
  ( Parser,
    braces,
    identifier,
    reserved,
    symbol,
  )

-- | Parses a config block.
-- Example of PSL config block:
-- datasource db {
--   provider   = "postgresql"
--   url        = env("DATABASE_URL")
--   extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
-- }
configBlock :: Parser Psl.ConfigBlock.ConfigBlock
configBlock = do
  configBlockType <-
    choice
      [ try (reserved "datasource" >> return Psl.ConfigBlock.Datasource),
        try (reserved "generator" >> return Psl.ConfigBlock.Generator)
      ]
  name <- identifier
  Psl.ConfigBlock.ConfigBlock configBlockType name <$> configBlockBody

configBlockBody :: Parser [Psl.ConfigBlock.KeyValuePair]
configBlockBody = braces (many keyValuePair)

-- | Parses a key-value pair.
-- Example of PSL key-value pair:
-- provider = "postgresql"
-- It works for both datasources and generators.
keyValuePair :: Parser Psl.ConfigBlock.KeyValuePair
keyValuePair = do
  key <- identifier
  _ <- symbol "="
  value <- expression
  return $ Psl.ConfigBlock.KeyValuePair key value
