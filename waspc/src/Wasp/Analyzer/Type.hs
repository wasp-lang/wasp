module Wasp.Analyzer.Type
  ( Type (..),
    DictEntryType (..),
    dictEntryRequired,
  )
where

import qualified Data.HashMap.Strict as H
import Data.List (intercalate)

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
  | TupleType (Type, Type, [Type])
  | StringType
  | NumberType
  | BoolType
  | ExtImportType
  | QuoterType String
  deriving (Eq)

instance Show Type where
  show = \case
    DeclType typeName -> typeName ++ " (declaration type)"
    EnumType typeName -> typeName ++ " (enum type)"
    DictType keyValueMap ->
      let showEntry (k, DictRequired v) = "  " ++ k ++ ": " ++ show v
          showEntry (k, DictOptional v) = "  " ++ k ++ "?: " ++ show v
       in case H.toList keyValueMap of
            [entry] -> "{" ++ showEntry entry ++ "}"
            entries -> "{\n" ++ intercalate ",\n" (map (("  " ++) . showEntry) entries) ++ "\n}"
    ListType typ -> "[" ++ show typ ++ "]"
    EmptyListType -> "[]"
    TupleType (t1, t2, ts) -> "(" ++ intercalate ", " (show <$> (t1 : t2 : ts)) ++ ")"
    StringType -> "string"
    NumberType -> "number"
    BoolType -> "bool"
    ExtImportType -> "external import"
    QuoterType tag -> "{=" ++ tag ++ " " ++ tag ++ "=}"

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
