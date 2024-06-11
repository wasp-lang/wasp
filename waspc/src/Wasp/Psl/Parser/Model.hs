module Wasp.Psl.Parser.Model
  ( parseModelBody,
    model,
    body,
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
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Parser.Attribute (attribute, blockAttribute)
import Wasp.Psl.Parser.Common
  ( braces,
    identifier,
    parens,
    reserved,
    stringLiteral,
    symbol,
    whiteSpace,
  )

type SourceCode = String

parseModelBody :: SourceCode -> Either Parsec.ParseError Psl.Model.ModelBody
parseModelBody = Parsec.parse Wasp.Psl.Parser.Model.body ""

-- | Parses PSL (Prisma Schema Language model).
-- Example of PSL model:
--   model User {
--     id Int @id
--     name String
--     @@index([name])
--   }
model :: Parser Psl.Schema.SchemaElement
model = do
  whiteSpace
  reserved "model"
  modelName <- identifier
  Psl.Schema.SchemaModel . Psl.Model.Model modelName <$> braces body

-- | Parses body of the PSL (Prisma Schema Language) model,
-- which is everything besides model keyword, name and braces:
--   `model User { <body> }`.
body :: Parser Psl.Model.ModelBody
body = do
  whiteSpace
  Psl.Model.ModelBody <$> many1 element

element :: Parser Psl.Model.ModelElement
element =
  try (Psl.Model.ModelElementField <$> field)
    <|> try (Psl.Model.ModelElementBlockAttribute <$> blockAttribute)

field :: Parser Psl.Model.ModelField
field = do
  name <- identifier
  type' <- fieldType
  maybeTypeModifier <- fieldTypeModifier
  attrs <- many (try attribute)
  return $
    Psl.Model.ModelField
      { Psl.Model._name = name,
        Psl.Model._type = type',
        Psl.Model._typeModifiers = maybeToList maybeTypeModifier,
        Psl.Model._attrs = attrs
      }
  where
    fieldType :: Parser Psl.Model.ModelFieldType
    fieldType =
      scalarFieldType
        <|> try
          ( Psl.Model.Unsupported
              <$> ( symbol "Unsupported"
                      >> parens stringLiteral
                  )
          )
        <|> Psl.Model.UserType <$> identifier

    scalarFieldType :: Parser Psl.Model.ModelFieldType
    scalarFieldType =
      foldl1
        (<|>)
        ( map
            -- Supported scalar types from https://github.com/prisma/prisma-engines/blob/main/psl/parser-database/src/types.rs#L1429
            (\(s, t) -> try (reserved s) >> return t)
            [ ("String", Psl.Model.String),
              ("Boolean", Psl.Model.Boolean),
              ("Int", Psl.Model.Int),
              ("BigInt", Psl.Model.BigInt),
              ("Float", Psl.Model.Float),
              ("Decimal", Psl.Model.Decimal),
              ("DateTime", Psl.Model.DateTime),
              ("Json", Psl.Model.Json),
              ("Bytes", Psl.Model.Bytes)
            ]
        )

    -- NOTE: As is Prisma currently implemented, there can be only one type modifier at one time: [] or ?.
    fieldTypeModifier :: Parser (Maybe Psl.Model.ModelFieldTypeModifier)
    fieldTypeModifier =
      optionMaybe
        ( (try (symbol "[]?") >> return Psl.Model.UnsupportedOptionalList)
            <|> (try (symbol "[]") >> return Psl.Model.List)
            <|> (try (symbol "?") >> return Psl.Model.Optional)
        )
