module Wasp.Psl.Ast.ConfigBlock
  ( Datasource (..),
    Generator (..),
    ConfigBlockKeyValue (..),
    IsConfigBlock (..),
  )
where

data Datasource
  = Datasource
      String
      [ConfigBlockKeyValue]
  deriving (Show, Eq)

instance IsConfigBlock Datasource where
  getConfigBlockName (Datasource name _) = name
  getConfigBlockKeyValues (Datasource _ keyValues) = keyValues

data Generator
  = Generator
      String
      [ConfigBlockKeyValue]
  deriving (Show, Eq)

instance IsConfigBlock Generator where
  getConfigBlockName (Generator name _) = name
  getConfigBlockKeyValues (Generator _ keyValues) = keyValues

-- | Represents a key-value pair in a config block.
--  For example, in the following config block:
--  ```
--  generator client {
--    provider = "prisma-client-js"
--  }
--  ```
--  The key-value pair would be `ConfigBlockKeyValue "provider" "prisma-client-js"`.
data ConfigBlockKeyValue = ConfigBlockKeyValue String String
  deriving (Show, Eq)

class IsConfigBlock a where
  getConfigBlockName :: a -> String
  getConfigBlockKeyValues :: a -> [ConfigBlockKeyValue]
