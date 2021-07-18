module Analyzer.TypeChecker
  ( TypedAST (..),
    TypedStmt (..),
    TypedExpr (..),
    TypeError,
    exprType,
    typeCheck,
  )
where

import Analyzer.Parser (AST, ExtImportName, Ident)
import Analyzer.Type (Type (..))
import Analyzer.TypeDefinitions (TypeDefinitions)

-- | Checks that an AST conforms to the type rules of Wasp and produces a
--   an AST labelled with type information.
typeCheck :: TypeDefinitions -> AST -> Either TypeError TypedAST
typeCheck _ _ = Right $ TypedAST {typedStmts = []}

-- TODO: instead of having separate AST for type checker, give `Parser.AST` a
-- "content" type argument that type information can be attached to

newtype TypedAST = TypedAST {typedStmts :: [TypedStmt]}

data TypedStmt = Decl Ident TypedExpr Type

data TypedExpr
  = Dict [(Ident, TypedExpr)] Type
  | List [TypedExpr] Type
  | StringLiteral String
  | IntegerLiteral Integer
  | DoubleLiteral Double
  | BoolLiteral Bool
  | ExtImport ExtImportName String
  | Identifier Ident Type
  | -- TODO: What type to represent these?
    JSON String
  | PSL String

-- | Given a `TypedExpr`, determines its `Type`.
exprType :: TypedExpr -> Type
exprType (Dict _ t) = t
exprType (List _ t) = t
exprType (StringLiteral _) = StringType
exprType (IntegerLiteral _) = NumberType
exprType (DoubleLiteral _) = NumberType
exprType (BoolLiteral _) = BoolType
exprType (ExtImport _ _) = ExtImportType
exprType (Identifier _ t) = t
exprType (JSON _) = QuoterType "json"
exprType (PSL _) = QuoterType "psl"

data TypeError
