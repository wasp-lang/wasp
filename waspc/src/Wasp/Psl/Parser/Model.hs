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
import qualified Wasp.Psl.Ast.Model as Model
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
model :: Parser Model.SchemaElement
model = do
  whiteSpace
  _ <- reserved "model"
  modelName <- identifier
  Model.SchemaModel . Model.Model modelName <$> braces modelBody

-- | Parses body of the PSL (Prisma Schema Language) model,
-- which is everything besides model keyword, name and braces:
--   `model User { <body> }`.
modelBody :: Parser Model.Body
modelBody = do
  whiteSpace
  Model.Body <$> many1 modelElement

modelElement :: Parser Model.Element
modelElement =
  try (Model.ElementField <$> modelField)
    <|> try (Model.ElementBlockAttribute <$> modelBlockAttribute)

modelField :: Parser Model.Field
modelField = do
  name <- identifier
  type' <- fieldType
  maybeTypeModifier <- fieldTypeModifier
  attrs <- many (try modelFieldAttribute)
  return $
    Model.Field
      { Model._name = name,
        Model._type = type',
        Model._typeModifiers = maybeToList maybeTypeModifier,
        Model._attrs = attrs
      }
  where
    fieldType :: Parser Model.FieldType
    fieldType =
      foldl1
        (<|>)
        ( map
            (\(s, t) -> try (symbol s) >> return t)
            [ ("String", Model.String),
              ("Boolean", Model.Boolean),
              ("Int", Model.Int),
              ("BigInt", Model.BigInt),
              ("Float", Model.Float),
              ("Decimal", Model.Decimal),
              ("DateTime", Model.DateTime),
              ("Json", Model.Json),
              ("Bytes", Model.Bytes)
            ]
        )
        <|> try
          ( Model.Unsupported
              <$> ( symbol "Unsupported"
                      >> parens stringLiteral
                  )
          )
        <|> Model.UserType <$> identifier

    -- NOTE: As is Prisma currently implemented, there can be only one type modifier at one time: [] or ?.
    fieldTypeModifier :: Parser (Maybe Model.FieldTypeModifier)
    fieldTypeModifier =
      optionMaybe
        ( (try (brackets whiteSpace) >> return Model.List)
            <|> (try (symbol "?") >> return Model.Optional)
        )

modelFieldAttribute :: Parser Model.Attribute
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
    Model.Attribute
      { Model._attrName = case maybeSelector of
          Just selector -> name ++ "." ++ selector
          Nothing -> name,
        Model._attrArgs = fromMaybe [] maybeArgs
      }

-- Parses attribute argument that ends with delimiter: , or ).
-- Doesn't parse the delimiter.
modelAttrArgument :: Parser Model.AttributeArg
modelAttrArgument = do
  try namedArg <|> try unnamedArg
  where
    namedArg :: Parser Model.AttributeArg
    namedArg = do
      name <- identifier
      _ <- colon
      Model.AttrArgNamed name <$> argValue

    unnamedArg :: Parser Model.AttributeArg
    unnamedArg = Model.AttrArgUnnamed <$> argValue

    argValue :: Parser Model.AttrArgValue
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

    argValueString :: Parser Model.AttrArgValue
    argValueString = Model.AttrArgString <$> stringLiteral

    argValueFunc :: Parser Model.AttrArgValue
    argValueFunc = do
      -- TODO: Could I implement this with applicative?
      name <- identifier
      parens whiteSpace
      return $ Model.AttrArgFunc name

    argValueFieldReferenceList :: Parser Model.AttrArgValue
    argValueFieldReferenceList =
      Model.AttrArgFieldRefList
        <$> brackets (commaSep1 identifier)

    -- NOTE: For now we are not supporting negative numbers.
    --   I couldn't figure out from Prisma docs if there could be the case
    --   where these numbers could be negative.
    --   Same goes for argValueNumberInt below.
    --   TODO: Probably we should take care of that case.
    argValueNumberFloat :: Parser Model.AttrArgValue
    argValueNumberFloat = Model.AttrArgNumber . show <$> float

    -- NOTE/TODO: Check comment on argValueNumberFloat.
    argValueNumberInt :: Parser Model.AttrArgValue
    argValueNumberInt = Model.AttrArgNumber . show <$> integer

    argValueIdentifier :: Parser Model.AttrArgValue
    argValueIdentifier = Model.AttrArgIdentifier <$> identifier

    argValueUnknown :: Parser Model.AttrArgValue
    argValueUnknown =
      Model.AttrArgUnknown <$> many1 (try $ noneOf argDelimiters)

    delimitedArgValue :: Parser Model.AttrArgValue -> Parser Model.AttrArgValue
    delimitedArgValue argValueP = do
      value <- argValueP
      _ <- lookAhead $ oneOf argDelimiters
      return value

    argDelimiters = [',', ')']

modelBlockAttribute :: Parser Model.Attribute
modelBlockAttribute = char '@' >> modelFieldAttribute
