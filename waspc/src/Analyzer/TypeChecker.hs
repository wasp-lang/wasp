module Analyzer.TypeChecker
  ( TypedAST (..)
  , TypedStmt (..)
  , TypedExpr (..)
  , TypeError
  , exprType
  , typeCheck
  ) where

import Analyzer.Syntax (AST, Ident, ExtImportName)
import Analyzer.Type (Type (..))
import Analyzer.Lib (Lib)

newtype TypedAST = TypedAST { typedStmts :: [TypedStmt] }

data TypedStmt = Decl Ident TypedExpr Type

data TypedExpr = Dict [(Ident, TypedExpr)] Type
               | List [TypedExpr] Type
               | StringLiteral String
               | IntegerLiteral Integer
               | DoubleLiteral Double
               | ExtImport ExtImportName String
               | Var Ident Type
               -- TODO: What type to represent these?
               | JSON String
               | PSL String

-- | Given a `TypedExpr`, determines its `Type`.
exprType :: TypedExpr -> Type
exprType (Dict _ t) = t
exprType (List _ t) = t
exprType (StringLiteral _) = StringType
exprType (IntegerLiteral _) = NumberType
exprType (DoubleLiteral _) = NumberType
exprType (ExtImport _ _) = ExtImportType
exprType (Var _ t) = t
exprType (JSON _) = JSONType
exprType (PSL _) = PSLType

data TypeError

-- | Checks that an AST conforms to the type rules of Wasp and produces a
--   an AST labelled with type information.
typeCheck :: Lib -> AST -> Either TypeError TypedAST
typeCheck _ _ = Right $ TypedAST { typedStmts = [] }
