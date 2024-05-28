module Wasp.Psl.Parser.Model
  ( parsePslBody,
    model,
    modelBody,
  )
where

import Data.Maybe (maybeToList)
import Text.Parsec
  ( many,
    many1,
    optionMaybe,
    try,
    (<|>),
  )
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
import Wasp.Psl.Parser.Common
  ( blockAttribute,
    braces,
    fieldAttribute,
    identifier,
    parens,
    reserved,
    stringLiteral,
    symbol,
    whiteSpace,
  )

parsePslBody :: String -> Either Parsec.ParseError Psl.Ast.Body
parsePslBody = Parsec.parse Wasp.Psl.Parser.Model.modelBody ""

-- | Parses PSL (Prisma Schema Language model).
-- Example of PSL model:
--   model User {
--     id Int @id
--     name String
--     @@index([name])
--   }
model :: Parser Psl.Ast.SchemaElement
model = do
  whiteSpace
  reserved "model"
  modelName <- identifier
  Psl.Ast.SchemaModel . Psl.Ast.Model modelName <$> braces modelBody

-- | Parses body of the PSL (Prisma Schema Language) model,
-- which is everything besides model keyword, name and braces:
--   `model User { <body> }`.
modelBody :: Parser Psl.Ast.Body
modelBody = do
  whiteSpace
  Psl.Ast.Body <$> many1 modelElement

modelElement :: Parser Psl.Ast.Element
modelElement =
  try (Psl.Ast.ElementField <$> modelField)
    <|> try (Psl.Ast.ElementBlockAttribute <$> blockAttribute)

modelField :: Parser Psl.Ast.Field
modelField = do
  name <- identifier
  type' <- fieldType
  maybeTypeModifier <- fieldTypeModifier
  attrs <- many (try fieldAttribute)
  return $
    Psl.Ast.Field
      { Psl.Ast._name = name,
        Psl.Ast._type = type',
        Psl.Ast._typeModifiers = maybeToList maybeTypeModifier,
        Psl.Ast._attrs = attrs
      }
  where
    fieldType :: Parser Psl.Ast.FieldType
    fieldType =
      scalarFieldType
        <|> try
          ( Psl.Ast.Unsupported
              <$> ( symbol "Unsupported"
                      >> parens stringLiteral
                  )
          )
        <|> Psl.Ast.UserType <$> identifier

    scalarFieldType :: Parser Psl.Ast.FieldType
    scalarFieldType =
      foldl1
        (<|>)
        ( map
            -- Supported scalar types from https://github.com/prisma/prisma-engines/blob/main/psl/parser-database/src/types.rs#L1429
            (\(s, t) -> try (reserved s) >> return t)
            [ ("String", Psl.Ast.String),
              ("Boolean", Psl.Ast.Boolean),
              ("Int", Psl.Ast.Int),
              ("BigInt", Psl.Ast.BigInt),
              ("Float", Psl.Ast.Float),
              ("Decimal", Psl.Ast.Decimal),
              ("DateTime", Psl.Ast.DateTime),
              ("Json", Psl.Ast.Json),
              ("Bytes", Psl.Ast.Bytes)
            ]
        )

    -- NOTE: As is Prisma currently implemented, there can be only one type modifier at one time: [] or ?.
    fieldTypeModifier :: Parser (Maybe Psl.Ast.FieldTypeModifier)
    fieldTypeModifier =
      optionMaybe
        ( (try (symbol "[]?") >> return Psl.Ast.UnsupportedOptionalList)
            <|> (try (symbol "[]") >> return Psl.Ast.List)
            <|> (try (symbol "?") >> return Psl.Ast.Optional)
        )
