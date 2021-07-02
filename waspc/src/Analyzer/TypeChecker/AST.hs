module Analyzer.TypeChecker.AST
  ( TypedAST (..),
    TypedStmt (..),
    TypedExpr (..),
    exprType,
    TypeError (..),
    TypeCoerceReason (..),
  )
where

import Analyzer.Parser (ExtImportName, Ident)
import Analyzer.Type

newtype TypedAST = TypedAST {typedStmts :: [TypedStmt]} deriving (Eq, Show)

data TypedStmt = Decl Ident TypedExpr Type deriving (Eq, Show)

data TypedExpr
  = Dict [(Ident, TypedExpr)] Type
  | List [TypedExpr] Type
  | StringLiteral String
  | IntegerLiteral Integer
  | DoubleLiteral Double
  | BoolLiteral Bool
  | ExtImport ExtImportName String
  | Var Ident Type
  | -- TODO: What type to represent these?
    JSON String
  | PSL String
  deriving (Eq, Show)

-- | Given a @TypedExpr@, determines its @Type@.
exprType :: TypedExpr -> Type
exprType (Dict _ t) = t
exprType (List _ t) = t
exprType (StringLiteral _) = StringType
exprType (IntegerLiteral _) = NumberType
exprType (DoubleLiteral _) = NumberType
exprType (BoolLiteral _) = BoolType
exprType (ExtImport _ _) = ExtImportType
exprType (Var _ t) = t
exprType (JSON _) = QuoterType "json"
exprType (PSL _) = QuoterType "psl"

data TypeError
  = UnificationError TypeCoerceReason Type Type
  | WeakenError TypeCoerceReason TypedExpr Type
  | NoDeclarationType String
  | UndefinedIdentifier String
  | QuoterUnknownTag String
  | QuoterDifferentTags String String
  | DictDuplicateField String
  | -- | Temperory "solution" to missing type inference for empty lists
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
  | -- | A coercion of the type contained in a list fialed
    ReasonList TypeError
  | -- | A coercion failed because a dictionary was missing a key
    ReasonDictNoKey String
  | -- | A weakening failed because a dictionary contained an extra key
    ReasonDictExtraKey String
  | -- | A coercion failed because two dictionaries had uncoercable types for a key
    ReasonDictWrongKeyType String TypeError
  deriving (Eq, Show)
