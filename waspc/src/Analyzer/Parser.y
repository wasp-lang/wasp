{
module Analyzer.Parser
  ( parse
  ) where

import Analyzer.Lexer
import Analyzer.Syntax
import Analyzer.ParserUtil (Parser, initialState, ParseState (..))
import Control.Monad.Trans.State.Lazy (evalStateT, get)
import Control.Monad.Trans.Except (throwE, runExcept)
import Control.Monad.Trans.Class (lift)
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%monad { Parser }
%lexer { lexer } { Token { tokenClass = TEOF } }

%token
  import { Token { tokenClass = TImport } }
  from { Token { tokenClass = TFrom } }
  string { Token { tokenClass = TString $$ } }
  int { Token { tokenClass = TInt $$ } }
  double { Token { tokenClass = TDouble $$ } }
  true { Token { tokenClass = TTrue } }
  false { Token { tokenClass = TFalse } }
  quoter {Token { tokenClass =  TQuoter $$ } }
  ident { Token { tokenClass = TIdent $$ } }
  '{' { Token { tokenClass = TLCurly } }
  '}' { Token { tokenClass = TRCurly } }
  ',' { Token { tokenClass = TComma } }
  ':' { Token { tokenClass = TColon } }
  '[' { Token { tokenClass = TLSquare } }
  ']' { Token { tokenClass = TRSquare } }

%%

Wasp : Stmt { AST [$1] }
     | Wasp Stmt { AST $ astStmts $1 ++ [$2] }

Stmt : Decl { $1 }
Decl : ident ident Expr { Decl $1 $2 $3 }

Expr : Dict { $1 }
     | List { $1 }
     | Extimport { $1 }
     | quoter { let (open, body, close) = $1 in Quoter open body close }
     | string { StringLiteral $1 }
     | int { IntegerLiteral $1 }
     | double { DoubleLiteral $1 }
     | true { BoolLiteral True }
     | false { BoolLiteral False }
     | ident { Identifier $1 }

Dict : '{' DictEntries '}' { Dict $2 }
     | '{' DictEntries ',' '}' { Dict $2 }
     | '{' '}' { Dict [] }
DictEntries : DictEntry { [$1] }
            | DictEntries ',' DictEntry { $1 ++ [$3] }
DictEntry : ident ':' Expr { ($1, $3) }

List : '[' ListVals ']' { List $2 }
     | '[' ListVals ',' ']' { List $2 }
     |  '[' ']' { List [] }
ListVals : Expr { [$1] }
         | ListVals ',' Expr { $1 ++ [$3] }

Extimport : import Name from string { ExtImport $2 $4 }
Name : ident { ExtImportModule $1 }
     | '{' ident '}' { ExtImportField $2 }

{
parseError :: Token -> Parser a
parseError token = lift $ throwE $ ParseError token

parse :: String -> Either ParseError AST
parse source = runExcept $ evalStateT parseTokens $ initialState source
}
