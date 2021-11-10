module Wasp.Analyzer.Type
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
  | -- | A temporary type given to an empty list. All occurences of this type
    -- will have been changed to an appropriate "ListType" when "typeCheck" is
    -- finished. See section 2.2 of the wasplang document for more information
    -- on this.
    EmptyListType
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
