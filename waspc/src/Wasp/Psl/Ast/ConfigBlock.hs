module Wasp.Psl.Ast.ConfigBlock
  ( ConfigBlock (..),
    ConfigBlockType (..),
    KeyValuePair (..),
    Identifier,
    overrideKeyValuePairs,
  )
where

import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (nubBy)
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import Wasp.Psl.Ast.AttachedComment (AttachedComment)
import Wasp.Psl.Ast.Common (Name)

-- | Represents a config block in the PSL.
--   For example, in the following PSL:
--   ```
--   generator client {
--     provider = "prisma-client-js"
--   }
--   ```
--   The config block would be `ConfigBlock Generator "client" [KeyValuePair "provider" "\"prisma-client-js\""]`.
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
  { _type :: ConfigBlockType,
    _name :: Name,
    _keyValuePairs :: [KeyValuePair],
    _configAttachedComments :: [AttachedComment]
  }
  deriving (Show, Eq)

data ConfigBlockType = Datasource | Generator
  deriving (Show, Eq)

type Identifier = String

-- | Represents a key-value pair in a config block.
--  For example, in the following config block:
--  ```
--  generator client {
--    provider = "prisma-client-js"
--  }
--  ```
--  The key-value pair would be `KeyValuePair "provider" "prisma-client-js"`.
data KeyValuePair = KeyValuePair
  { _key :: Identifier,
    _value :: Psl.Argument.Expression,
    _attachedComments :: [AttachedComment]
  }
  deriving (Show, Eq)

overrideKeyValuePairs :: [(String, Psl.Argument.Expression)] -> ConfigBlock -> ConfigBlock
overrideKeyValuePairs
  overridePairs
  (ConfigBlock configBlockType name originalKeyValues attachedComments) =
    ConfigBlock configBlockType name overridenKeyValues attachedComments
    where
      overrideKeyValues =
        overridePairs <&> (\(key, value) -> KeyValuePair key value [])

      overridenKeyValues =
        nubBy ((==) `on` _key) (overrideKeyValues ++ originalKeyValues)
