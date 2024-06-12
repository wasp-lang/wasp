module Wasp.Psl.Ast.ConfigBlock
  ( ConfigBlock (..),
    ConfigBlockType (..),
    ConfigBlockKeyValue (..),
    configBlockName,
    configBlockKeyValues,
  )
where

type Name = String

-- | Represents a config block in the PSL.
--   For example, in the following PSL:
--   ```
--   generator client {
--     provider = "prisma-client-js"
--   }
--   ```
--   The config block would be `ConfigBlock Generator "client" [ConfigBlockKeyValue "provider" "\"prisma-client-js\""]`.
--   Another example:
--   ```
--   datasource db {
--     provider   = "postgresql"
--     url        = env("DATABASE_URL")
--     extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
--   }
--   ```
--   The config block would be `ConfigBlock Datasource "db" [ ... ]`.
data ConfigBlock = ConfigBlock ConfigBlockType Name [ConfigBlockKeyValue]
  deriving (Show, Eq)

configBlockName :: ConfigBlock -> Name
configBlockName (ConfigBlock _ name _) = name

configBlockKeyValues :: ConfigBlock -> [ConfigBlockKeyValue]
configBlockKeyValues (ConfigBlock _ _ keyValues) = keyValues

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
--  The key-value pair would be `ConfigBlockKeyValue "provider" "prisma-client-js"`.
data ConfigBlockKeyValue = ConfigBlockKeyValue Identifier Value
  deriving (Show, Eq)
