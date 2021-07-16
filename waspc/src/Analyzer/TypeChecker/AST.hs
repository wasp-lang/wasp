module Analyzer.TypeChecker.AST
  ( TypedAST (..),
    TypedStmt (..),
    TypedExpr (..),
    Ident,
    ExtImportName,
    exprType,
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
