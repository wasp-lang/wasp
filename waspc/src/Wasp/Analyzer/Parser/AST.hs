module Wasp.Analyzer.Parser.AST
  ( AST (..),
    Stmt (..),
    Expr (..),
    Identifier,
    ExtImportName (..),
  )
where

import Wasp.Analyzer.Parser.Ctx
import Wasp.AppSpec.ExtImport (ExtImportName (..))

newtype AST = AST {astStmts :: [WithCtx Stmt]} deriving (Eq, Show)

data Stmt
  = -- | Decl <declType> <declName> <declBody>
    Decl Identifier Identifier (WithCtx Expr)
  deriving (Eq, Show)

{- ORMOLU_DISABLE -}
data Expr
  = Dict           [(Identifier, WithCtx Expr)]
  | List           [WithCtx Expr]
  | Tuple          (WithCtx Expr, WithCtx Expr, [WithCtx Expr])
  | StringLiteral  String
  | IntegerLiteral Integer
  | DoubleLiteral  Double
  | BoolLiteral    Bool
  | ExtImport      ExtImportName String
  | Var            Identifier
  | Quoter         Identifier String
  deriving (Eq, Show)
{- ORMOLU_ENABLE -}

type Identifier = String
