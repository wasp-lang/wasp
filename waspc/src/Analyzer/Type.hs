module Analyzer.Type
  ( Type (..),
    DictEntryType (..),
    dictEntryRequired,
  )
where

import qualified Data.HashMap.Strict as H

-- | All possible types in Wasp.
data Type
  = DeclType String
  | EnumType String
  | DictType (H.HashMap String DictEntryType)
  | ListType Type
  | StringType
  | NumberType
  | BoolType
  | ExtImportType
  | QuoterType String
  deriving (Eq, Show)

-- | The type of an entry in a `Dict`.
data DictEntryType
  = DictRequired {dictEntryType :: Type}
  | DictOptional {dictEntryType :: Type}
  deriving (Eq, Show)

-- | Determines whether the entry must be present in an instance of its parent
--   `Dict` type.
dictEntryRequired :: DictEntryType -> Bool
dictEntryRequired DictRequired {} = True
dictEntryRequired DictOptional {} = False
