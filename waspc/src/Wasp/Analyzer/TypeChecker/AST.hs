module Wasp.Analyzer.TypeChecker.AST
  ( TypedAST (..),
    TypedStmt (..),
    TypedExpr (..),
    Identifier,
    ExtImportName (..),
    WithCtx (..),
    withCtx,
    exprType,
  )
where

import Wasp.Analyzer.Parser (ExtImportName (..), Identifier)
import Wasp.Analyzer.Parser.Ctx (WithCtx (..), withCtx)
import Wasp.Analyzer.Type

newtype TypedAST = TypedAST {typedStmts :: [WithCtx TypedStmt]} deriving (Eq, Show)

data TypedStmt
  = -- | Decl <declName> <declBody> <declType>
    Decl Identifier (WithCtx TypedExpr) Type
  deriving (Eq, Show)

{- ORMOLU_DISABLE -}
data TypedExpr
  = Dict           [(Identifier, WithCtx TypedExpr)] Type
  | List           [WithCtx TypedExpr] Type
  | Tuple          (WithCtx TypedExpr, WithCtx TypedExpr, [WithCtx TypedExpr]) Type
  | StringLiteral  String
  | IntegerLiteral Integer
  | DoubleLiteral  Double
  | BoolLiteral    Bool
  | ExtImport      ExtImportName String
  | Var            Identifier Type
  | -- TODO: When adding quoters to TypeDefinitions, these JSON/PSL variants will have to be changed
    JSON           String
  | PSL            String
  deriving (Eq, Show)
{- ORMOLU_ENABLE -}

-- | Given a @TypedExpr@, determines its @Type@.
exprType :: TypedExpr -> Type
exprType (Dict _ t) = t
exprType (List _ t) = t
exprType (Tuple _ t) = t
exprType (StringLiteral _) = StringType
exprType (IntegerLiteral _) = NumberType
exprType (DoubleLiteral _) = NumberType
exprType (BoolLiteral _) = BoolType
exprType (ExtImport _ _) = ExtImportType
exprType (Var _ t) = t
exprType (JSON _) = QuoterType "json"
exprType (PSL _) = QuoterType "psl"
