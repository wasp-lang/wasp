module Wasp.Psl.Parser.Argument
  ( argument,
    argumentList,
    expression,
    identifierStrParser,
  )
where

import Text.Megaparsec (choice, optional, try)
import qualified Text.Megaparsec.Char as C
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import Wasp.Psl.Parser.Common
  ( Parser,
    brackets,
    colon,
    commaSep,
    float,
    identifier,
    integer,
    parens,
    stringLiteral,
  )

argument :: Parser Psl.Argument.Argument
argument =
  choice
    [ try namedArg,
      try unnamedArg
    ]
  where
    namedArg :: Parser Psl.Argument.Argument
    namedArg = do
      name <- identifier
      _ <- colon
      Psl.Argument.ArgNamed name <$> expression

    unnamedArg :: Parser Psl.Argument.Argument
    unnamedArg = Psl.Argument.ArgUnnamed <$> expression

argumentList :: Parser [Psl.Argument.Argument]
argumentList = parens (commaSep (try argument))

expression :: Parser Psl.Argument.Expression
expression =
  choice $
    map
      try
      [ funcCallExpr,
        arrayExpr,
        numberFloatExpr,
        numberIntExpr,
        stringExpr,
        identifierExpr
      ]

stringExpr :: Parser Psl.Argument.Expression
stringExpr = Psl.Argument.StringExpr <$> stringLiteral

funcCallExpr :: Parser Psl.Argument.Expression
funcCallExpr =
  Psl.Argument.FuncExpr
    <$> identifier <*> argumentList

arrayExpr :: Parser Psl.Argument.Expression
arrayExpr =
  Psl.Argument.ArrayExpr
    <$> brackets (commaSep expression)

numberFloatExpr :: Parser Psl.Argument.Expression
numberFloatExpr = Psl.Argument.NumberExpr . show <$> float

numberIntExpr :: Parser Psl.Argument.Expression
numberIntExpr = Psl.Argument.NumberExpr . show <$> integer

-- NOTE: In order to support native database type attribute arguments, we need to support
--   identifiers that have dots in them, like "db.VarChar(200)".
--   We are not trying to be very smart here though: we don't check that "db" part matches
--   the name of the datasource block name (as it should), and we don't check that "VarChar" part is PascalCase
--   (as it should be) or that it is one of the valid values.
--   We just treat it as any other attribute, where "db.VarChar" becomes an attribute name.
--   In case that we wanted to be smarter, we could expand the AST to have special representation for it.
--   Also, we could do some additional checks here in parser (PascalCase), and some additional checks
--   in th generator ("db" matching the datasource block name).
identifierExpr :: Parser Psl.Argument.Expression
identifierExpr = Psl.Argument.IdentifierExpr <$> identifierStrParser

identifierStrParser :: Parser String
identifierStrParser = do
  first <- identifier
  rest <- optional (C.char '.' >> identifierStrParser)
  return $ maybe first ((first ++ ".") ++) rest
