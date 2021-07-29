module Analyzer.TypeChecker.TypeError
  ( TypeError (..),
    TypeCoerceReason (..),
  )
where

import Analyzer.Type
import Analyzer.TypeChecker.AST

data TypeError
  = UnificationError TypeCoerceReason Type Type
  | WeakenError TypeCoerceReason TypedExpr Type
  | NoDeclarationType String
  | UndefinedIdentifier String
  | QuoterUnknownTag String
  | DictDuplicateField String
  | -- | Temporary "solution" to missing type inference for empty lists
    EmptyListNotImplemented
  deriving (Eq, Show)

-- | Describes a reason that a @UnificationError@ or @WeakenError@ happened
data TypeCoerceReason
  = -- | A coercion involving a DeclType and a different type happened
    ReasonDecl
  | -- | A coercion involving an EnuMType and a different type happened
    ReasonEnum
  | -- | There is no relationship between the types in the coercion
    ReasonUncoercable
  | -- | A coercion of the type contained in a list failed
    ReasonList TypeError
  | -- | A coercion failed because a dictionary was missing a key
    ReasonDictNoKey String
  | -- | A weakening failed because a dictionary contained an extra key
    ReasonDictExtraKey String
  | -- | A coercion failed because two dictionaries had uncoercable types for a key
    ReasonDictWrongKeyType String TypeError
  deriving (Eq, Show)
