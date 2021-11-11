module Wasp.Analyzer.TypeChecker.TypeError
  ( TypeError (..),
    TypeCoerceReason (..),
  )
where

-- TODO:
-- TypeErrors are not detailed enough for the final version, missing is:
-- 1. Reporting multiple type errors:
--     This can be incrementally improved, e.g.
--     1. improve to 1 error per statement
--     2. improve to multiple errors per list/per dictionray
-- 2. Position information:
--     The start position in the source and end position should be recorded. (3) may
--     affect this, since it may be hard to say exactly where a unification or
--     weaken error happened.
-- 3. Informative error messages:
--     Make user-readable messages from all errors, and special care into explaining
--     unification and weaken errors (users shouldn't have to know what unification
--     and weakening is).

import Wasp.Analyzer.Type
import Wasp.Analyzer.TypeChecker.AST

data TypeError
  = UnificationError TypeCoerceReason Type Type
  | WeakenError TypeCoerceReason TypedExpr Type
  | NoDeclarationType String
  | UndefinedIdentifier String
  | QuoterUnknownTag String
  | DictDuplicateField String
  deriving (Eq, Show)

-- | Describes a reason that a @UnificationError@ or @WeakenError@ happened
data TypeCoerceReason
  = -- | A coercion involving a DeclType and a different type happened. For example,
    -- @unifyTypes (DeclType "foo") (DeclType "bar")@ and
    -- @unifyTypes (DeclType "foo") StringType@ would use this reason.
    ReasonDecl
  | -- | A coercion involving an EnumType and a different type happened. Similar to
    -- @ReasonDecl@, but for enum types.
    ReasonEnum
  | -- | There is no relationship between the types in the coercion
    ReasonUncoercable
  | -- | A coercion of the type contained in a list failed
    ReasonList TypeError
  | -- | A coercion failed because a dictionary was missing a key
    ReasonDictNoKey String
  | -- | A coercion failed because a dictionary contained an extra key
    ReasonDictExtraKey String
  | -- | A coercion failed because two dictionaries had uncoercable types for a key
    ReasonDictWrongKeyType String TypeError
  deriving (Eq, Show)
