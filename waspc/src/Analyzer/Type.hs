module Analyzer.Type where

-- | All possible types in Wasp.
data Type
  = DeclType String
  | EnumType String
  | DictType [DictEntryType]
  | ListType Type
  | StringType
  | NumberType
  | BoolType
  | ExtImportType
  | QuoterType String
  deriving (Eq, Show)

-- | The type of an entry in a `Dict`.
data DictEntryType
  = DictEntry {dictEntryName :: String, dictEntryType :: Type}
  | DictOptionalEntry {dictEntryName :: String, dictEntryType :: Type}
  deriving (Eq, Show)

-- | Determines whether the entry must be present in an instance of its parent
--   `Dict` type.
dictEntryRequired :: DictEntryType -> Bool
dictEntryRequired DictEntry {} = True
dictEntryRequired DictOptionalEntry {} = False
