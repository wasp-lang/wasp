module Wasp.Psl.Parser.Model
  ( model,
    modelBody,
    -- NOTE: Only for testing:
    modelAttrArgument,
  )
where

import Data.Maybe (fromMaybe, maybeToList)
import Text.Parsec
  ( char,
    choice,
    lookAhead,
    many,
    many1,
    noneOf,
    oneOf,
    optionMaybe,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
import Wasp.Psl.Parser.Common
  ( braces,
    brackets,
    colon,
    commaSep1,
    float,
    identifier,
    integer,
    parens,
    reserved,
    stringLiteral,
    symbol,
    whiteSpace,
  )

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
    <|> try (Psl.Ast.ElementBlockAttribute <$> modelBlockAttribute)

modelField :: Parser Psl.Ast.Field
modelField = do
  name <- identifier
  type' <- fieldType
  maybeTypeModifier <- fieldTypeModifier
  attrs <- many (try modelFieldAttribute)
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
      foldl1
        (<|>)
        ( map
            (\(s, t) -> try (symbol s) >> return t)
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
        <|> try
          ( Psl.Ast.Unsupported
              <$> ( symbol "Unsupported"
                      >> parens stringLiteral
                  )
          )
        <|> Psl.Ast.UserType <$> identifier

    -- NOTE: As is Prisma currently implemented, there can be only one type modifier at one time: [] or ?.
    fieldTypeModifier :: Parser (Maybe Psl.Ast.FieldTypeModifier)
    fieldTypeModifier =
      optionMaybe
        ( (try (brackets whiteSpace) >> return Psl.Ast.List)
            <|> (try (symbol "?") >> return Psl.Ast.Optional)
        )

modelFieldAttribute :: Parser Psl.Ast.Attribute
modelFieldAttribute = do
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

  maybeArgs <- optionMaybe (parens (commaSep1 (try modelAttrArgument)))
  return $
    Psl.Ast.Attribute
      { Psl.Ast._attrName = case maybeSelector of
          Just selector -> name ++ "." ++ selector
          Nothing -> name,
        Psl.Ast._attrArgs = fromMaybe [] maybeArgs
      }

-- Parses attribute argument that ends with delimiter: , or ).
-- Doesn't parse the delimiter.
modelAttrArgument :: Parser Psl.Ast.AttributeArg
modelAttrArgument = do
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

modelBlockAttribute :: Parser Psl.Ast.Attribute
modelBlockAttribute = char '@' >> modelFieldAttribute
