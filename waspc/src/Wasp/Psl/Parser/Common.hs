module Wasp.Psl.Parser.Common where

import Data.Maybe (fromMaybe)
import Text.Parsec
  ( alphaNum,
    char,
    choice,
    letter,
    lookAhead,
    many1,
    noneOf,
    oneOf,
    optionMaybe,
    try,
    (<|>),
  )
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T
import qualified Wasp.Psl.Ast.Schema as Psl.Ast

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

identifier :: Parser String
identifier = T.identifier lexer

braces :: Parser a -> Parser a
braces = T.braces lexer

symbol :: String -> Parser String
symbol = T.symbol lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

stringLiteral :: Parser String
stringLiteral = T.stringLiteral lexer

brackets :: Parser a -> Parser a
brackets = T.brackets lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 lexer

colon :: Parser String
colon = T.colon lexer

float :: Parser Double
float = T.float lexer

integer :: Parser Integer
integer = T.integer lexer

lexer :: T.TokenParser ()
lexer =
  T.makeTokenParser
    emptyDef
      { T.commentLine = "//",
        T.caseSensitive = True,
        T.identStart = letter,
        T.identLetter = alphaNum <|> char '_',
        T.reservedNames = ["model", "type", "view", "enum", "generator", "datasource"]
      }

fieldAttribute :: Parser Psl.Ast.Attribute
fieldAttribute = do
  _ <- char '@'
  name <- identifier
  -- NOTE: we support potential "selector" in order to support native database type attributes.
  --   These have names with single . in them, like this: @db.VarChar(200), @db.TinyInt(1), ... .
  --   We are not trying to be very smart here though: we don't check that "db" part matches
  --   the name of the datasource block name (as it should), and we don't check that "VarChar" part is PascalCase
  --   (as it should be) or that it is one of the valid values.
  --   We just treat it as any other attribute, where "db.VarChar" becomes an attribute name.
  --   In case that we wanted to be smarter, we could expand the AST to have special representation for it.
  --   Also, we could do some additional checks here in parser (PascalCase), and some additional checks
  --   in th generator ("db" matching the datasource block name).
  maybeSelector <- optionMaybe $ try $ char '.' >> identifier

  maybeArgs <- optionMaybe (parens (commaSep1 (try attrArgument)))
  return $
    Psl.Ast.Attribute
      { Psl.Ast._attrName = case maybeSelector of
          Just selector -> name ++ "." ++ selector
          Nothing -> name,
        Psl.Ast._attrArgs = fromMaybe [] maybeArgs
      }

-- Parses attribute argument that ends with delimiter: , or ).
-- Doesn't parse the delimiter.
attrArgument :: Parser Psl.Ast.AttributeArg
attrArgument = do
  try namedArg <|> try unnamedArg
  where
    namedArg :: Parser Psl.Ast.AttributeArg
    namedArg = do
      name <- identifier
      _ <- colon
      Psl.Ast.AttrArgNamed name <$> argValue

    unnamedArg :: Parser Psl.Ast.AttributeArg
    unnamedArg = Psl.Ast.AttrArgUnnamed <$> argValue

    argValue :: Parser Psl.Ast.AttrArgValue
    argValue =
      choice $
        map
          (try . delimitedArgValue)
          [ argValueString,
            argValueFunc,
            argValueFieldReferenceList,
            argValueNumberFloat,
            argValueNumberInt,
            argValueIdentifier,
            argValueUnknown
          ]

    argValueString :: Parser Psl.Ast.AttrArgValue
    argValueString = Psl.Ast.AttrArgString <$> stringLiteral

    argValueFunc :: Parser Psl.Ast.AttrArgValue
    argValueFunc = do
      -- TODO: Could I implement this with applicative?
      name <- identifier
      parens whiteSpace
      return $ Psl.Ast.AttrArgFunc name

    argValueFieldReferenceList :: Parser Psl.Ast.AttrArgValue
    argValueFieldReferenceList =
      Psl.Ast.AttrArgFieldRefList
        <$> brackets (commaSep1 identifier)

    -- NOTE: For now we are not supporting negative numbers.
    --   I couldn't figure out from Prisma docs if there could be the case
    --   where these numbers could be negative.
    --   Same goes for argValueNumberInt below.
    --   TODO: Probably we should take care of that case.
    argValueNumberFloat :: Parser Psl.Ast.AttrArgValue
    argValueNumberFloat = Psl.Ast.AttrArgNumber . show <$> float

    -- NOTE/TODO: Check comment on argValueNumberFloat.
    argValueNumberInt :: Parser Psl.Ast.AttrArgValue
    argValueNumberInt = Psl.Ast.AttrArgNumber . show <$> integer

    argValueIdentifier :: Parser Psl.Ast.AttrArgValue
    argValueIdentifier = Psl.Ast.AttrArgIdentifier <$> identifier

    argValueUnknown :: Parser Psl.Ast.AttrArgValue
    argValueUnknown =
      Psl.Ast.AttrArgUnknown <$> many1 (try $ noneOf argDelimiters)

    delimitedArgValue :: Parser Psl.Ast.AttrArgValue -> Parser Psl.Ast.AttrArgValue
    delimitedArgValue argValueP = do
      value <- argValueP
      _ <- lookAhead $ oneOf argDelimiters
      return value

    argDelimiters = [',', ')']

blockAttribute :: Parser Psl.Ast.Attribute
blockAttribute = char '@' >> fieldAttribute
