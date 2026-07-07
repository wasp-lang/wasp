module Wasp.Analyzer.AST
  ( AST (..),
    Stmt (..),
    Expr (..),
    Identifier,
    ExtImportName (..),
    isValidWaspIdentifier,
  )
where

import Data.Char (isAlpha, isAlphaNum)
import Wasp.Analyzer.Ctx
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

-- | Checks if a string is a valid wasp identifier.
--
-- A valid identifier matches the lexer rule @[_a-zA-Z][_a-zA-Z0-9]*'*@ (it
-- starts with a letter or underscore, continues with letters, digits or
-- underscores, and may end with any number of apostrophes) and is not one of
-- the reserved keywords.
isValidWaspIdentifier :: String -> Bool
isValidWaspIdentifier str = matchesIdentifierRule str && str `notElem` reservedKeywords
  where
    matchesIdentifierRule [] = False
    matchesIdentifierRule (c : cs) =
      isIdentStart c && all isIdentChar body && all (== '\'') primes
      where
        (body, primes) = span (/= '\'') cs
    isIdentStart ch = isAlpha ch || ch == '_'
    isIdentChar ch = isAlphaNum ch || ch == '_'
    -- Keywords that the lexer recognized as their own tokens rather than as
    -- identifiers.
    reservedKeywords = ["import", "from", "true", "false"]
