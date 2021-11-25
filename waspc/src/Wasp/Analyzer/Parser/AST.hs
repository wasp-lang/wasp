module Wasp.Analyzer.Parser.AST
  ( AST (..),
    Stmt (..),
    Expr (..),
    Identifier,
    ExtImportName (..),
  )
where

-- TODO: Am I ok with this? Would I rather import from `Evaluator.AppSpec.Types`?
-- But it doesn't make any sense any more to keep those in `Evaluator`, since they are also
-- used here now. I think we should just remove `Evaluator.AppSpec.Types` and import
-- stuff from AppSpec directly where we need it.
import Wasp.AppSpec.ExtImport (ExtImportName (..))

newtype AST = AST {astStmts :: [Stmt]} deriving (Eq, Show)

-- Decl <declType> <name> <body>
data Stmt = Decl Identifier Identifier Expr deriving (Eq, Show)

data Expr
  = Dict [(Identifier, Expr)]
  | List [Expr]
  | StringLiteral String
  | IntegerLiteral Integer
  | DoubleLiteral Double
  | BoolLiteral Bool
  | ExtImport ExtImportName String
  | Var Identifier
  | Quoter Identifier String
  deriving (Eq, Show)

type Identifier = String
