module Wasp.Psl.Ast.ConfigBlock
  ( ConfigBlock (..),
    ConfigBlockType (..),
    ConfigBlockKeyValuePair (..),
    overrideKeyValuePairs,
  )
where

import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (nubBy)
import Wasp.Psl.Ast.Common (Name)

-- | Represents a config block in the PSL.
--   For example, in the following PSL:
--   ```
--   generator client {
--     provider = "prisma-client-js"
--   }
--   ```
--   The config block would be `ConfigBlock Generator "client" [ConfigBlockKeyValuePair "provider" "\"prisma-client-js\""]`.
--   Another example:
--   ```
--   datasource db {
--     provider   = "postgresql"
--     url        = env("DATABASE_URL")
--     extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
--   }
--   ```
--   The config block would be `ConfigBlock Datasource "db" [ ... ]`.
data ConfigBlock = ConfigBlock
  { _configBlockType :: ConfigBlockType,
    _name :: Name,
    _keyValuePairs :: [ConfigBlockKeyValuePair]
  }
  deriving (Show, Eq)

data ConfigBlockType = Datasource | Generator
  deriving (Show, Eq)

type Identifier = String

type Value = String

-- | Represents a key-value pair in a config block.
--  For example, in the following config block:
--  ```
--  generator client {
--    provider = "prisma-client-js"
--  }
--  ```
--  The key-value pair would be `ConfigBlockKeyValuePair "provider" "prisma-client-js"`.
data ConfigBlockKeyValuePair = ConfigBlockKeyValuePair Identifier Value
  deriving (Show, Eq)

overrideKeyValuePairs :: [(String, String)] -> ConfigBlock -> ConfigBlock
overrideKeyValuePairs
  overridePairs
  (ConfigBlock configBlockType name originalKeyValues) =
    ConfigBlock configBlockType name overridenKeyValues
    where
      configBlockPairs =
        originalKeyValues
          <&> (\(ConfigBlockKeyValuePair key value) -> (key, value))
      overridenKeyValues =
        nubBy ((==) `on` fst) (overridePairs ++ configBlockPairs)
          <&> uncurry ConfigBlockKeyValuePair
