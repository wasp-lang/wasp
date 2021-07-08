{
-- This file is processed by Happy (https://www.haskell.org/happy/) and generates
-- the module `Analyzer.Parser.Parser`

module Analyzer.Parser.Parser
  ( parse
  ) where

import Analyzer.Parser.Lexer
import Analyzer.Parser.AST
import Analyzer.Parser.Token
import Analyzer.Parser.ParseError
import Analyzer.Parser.Util (Parser, initialState, ParserState (..))
import Control.Monad.Trans.State.Lazy (evalStateT, get)
import Control.Monad.Trans.Except (throwE, runExcept)
import Control.Monad.Trans.Class (lift)
}

-- These lines tell Happy to call the main parsing function `parse`, that it
-- will operate on the `Token` type, and to call `parseError` when the parser
-- encounters an error.
%name parse
%tokentype { Token }
%error { parseError }

-- This sets up Happy to use a monadic parser and threaded lexer
-- This means that Happy will request tokens as it needs them instead of
-- requiring a list of all tokens up front. A monad can also be used in the
-- lexer and parser to track state and errors.
%monad { Parser }
%lexer { lexer } { Token { tokenClass = TEOF } }

-- This section defines the names that are used in the grammar section to
-- refer to each type of token.

-- The "$$" means that the value of the token is the pattern in that location

%token
  import { Token { tokenClass = TImport } }
  from   { Token { tokenClass = TFrom } }
  string { Token { tokenClass = TString $$ } }
  int    { Token { tokenClass = TInt $$ } }
  double { Token { tokenClass = TDouble $$ } }
  true   { Token { tokenClass = TTrue } }
  false  { Token { tokenClass = TFalse } }
  quoter {Token { tokenClass =  TQuoter $$ } }
  ident  { Token { tokenClass = TIdentifier $$ } }
  '{'    { Token { tokenClass = TLCurly } }
  '}'    { Token { tokenClass = TRCurly } }
  ','    { Token { tokenClass = TComma } }
  ':'    { Token { tokenClass = TColon } }
  '['    { Token { tokenClass = TLSquare } }
  ']'    { Token { tokenClass = TRSquare } }

%%

Wasp :: { AST }
  : Stmt { AST [$1] }
  | Wasp Stmt { AST $ astStmts $1 ++ [$2] }

Stmt :: { Stmt }
  : Decl { $1 }
Decl :: { Stmt }
  : ident ident Expr { Decl $1 $2 $3 }

Expr :: { Expr }
  : Dict { $1 }
  | List { $1 }
  | Extimport { $1 }
  | quoter { let (tag, body) = $1 in Quoter tag body }
  | string { StringLiteral $1 }
  | int { IntegerLiteral $1 }
  | double { DoubleLiteral $1 }
  | true { BoolLiteral True }
  | false { BoolLiteral False }
  | ident { Identifier $1 }

Dict :: { Expr }
  : '{' DictEntries '}' { Dict $2 }
  | '{' DictEntries ',' '}' { Dict $2 }
  | '{' '}' { Dict [] }
DictEntries :: { [(Ident, Expr)] }
  : DictEntry { [$1] }
  | DictEntries ',' DictEntry { $1 ++ [$3] }
DictEntry :: { (Ident, Expr) }
  : ident ':' Expr { ($1, $3) }

List :: { Expr }
  : '[' ListVals ']' { List $2 }
  | '[' ListVals ',' ']' { List $2 }
  |  '[' ']' { List [] }
ListVals :: { [Expr] }
  : Expr { [$1] }
  | ListVals ',' Expr { $1 ++ [$3] }

Extimport :: { Expr }
  : import Name from string { ExtImport $2 $4 }
Name :: { ExtImportName }
  : ident { ExtImportModule $1 }
  | '{' ident '}' { ExtImportField $2 }

{
parseError :: Token -> Parser a
parseError token = lift $ throwE $ ParseError token
}
