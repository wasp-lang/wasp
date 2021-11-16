module Wasp.Psl.Parser.Model
  ( model,
    body,
    -- NOTE: Only for testing:
    attrArgument,
  )
where

import Data.Maybe (fromMaybe, maybeToList)
import Text.Parsec
  ( alphaNum,
    char,
    choice,
    letter,
    lookAhead,
    many,
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
import qualified Wasp.Psl.Ast.Model as Model

-- | Parses PSL (Prisma Schema Language model).
-- Example of PSL model:
--   model User {
--     id Int @id
--     name String
--     @@index([name])
--   }
model :: Parser Model.Model
model = do
  T.whiteSpace lexer
  _ <- T.symbol lexer "model"
  modelName <- T.identifier lexer
  Model.Model modelName <$> T.braces lexer body

-- | Parses body of the PSL (Prisma Schema Language) model,
-- which is everything besides model keyword, name and braces:
--   `model User { <body> }`.
body :: Parser Model.Body
body = do
  T.whiteSpace lexer
  Model.Body <$> many1 element

element :: Parser Model.Element
element =
  try (Model.ElementField <$> field)
    <|> try (Model.ElementBlockAttribute <$> blockAttribute)

field :: Parser Model.Field
field = do
  name <- T.identifier lexer
  type' <- fieldType
  maybeTypeModifier <- fieldTypeModifier
  attrs <- many (try attribute)
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
            (\(s, t) -> try (T.symbol lexer s) >> return t)
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
              <$> ( T.symbol lexer "Unsupported"
                      >> T.parens lexer (T.stringLiteral lexer)
                  )
          )
        <|> Model.UserType <$> T.identifier lexer

    -- NOTE: As is Prisma currently implemented, there can be only one type modifier at one time: [] or ?.
    fieldTypeModifier :: Parser (Maybe Model.FieldTypeModifier)
    fieldTypeModifier =
      optionMaybe
        ( (try (T.brackets lexer (T.whiteSpace lexer)) >> return Model.List)
            <|> (try (T.symbol lexer "?") >> return Model.Optional)
        )

attribute :: Parser Model.Attribute
attribute = do
  _ <- char '@'
  name <- T.identifier lexer
  -- NOTE: we support potential "selector" in order to support native database type attributes.
  --   These have names with single . in them, like this: @db.VarChar(200), @db.TinyInt(1), ... .
  --   We are not trying to be very smart here though: we don't check that "db" part matches
  --   the name of the datasource block name (as it should), and we don't check that "VarChar" part is PascalCase
  --   (as it should be) or that it is one of the valid values.
  --   We just treat it as any other attribute, where "db.VarChar" becomes an attribute name.
  --   In case that we wanted to be smarter, we could expand the AST to have special representation for it.
  --   Also, we could do some additional checks here in parser (PascalCase), and some additional checks
  --   in th generator ("db" matching the datasource block name).
  maybeSelector <- optionMaybe $ try $ char '.' >> T.identifier lexer

  maybeArgs <- optionMaybe (T.parens lexer (T.commaSep1 lexer (try attrArgument)))
  return $
    Model.Attribute
      { Model._attrName = case maybeSelector of
          Just selector -> name ++ "." ++ selector
          Nothing -> name,
        Model._attrArgs = fromMaybe [] maybeArgs
      }

-- Parses attribute argument that ends with delimiter: , or ).
-- Doesn't parse the delimiter.
attrArgument :: Parser Model.AttributeArg
attrArgument = do
  try namedArg <|> try unnamedArg
  where
    namedArg :: Parser Model.AttributeArg
    namedArg = do
      name <- T.identifier lexer
      _ <- T.colon lexer
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
    argValueString = Model.AttrArgString <$> T.stringLiteral lexer

    argValueFunc :: Parser Model.AttrArgValue
    argValueFunc = do
      -- TODO: Could I implement this with applicative?
      name <- T.identifier lexer
      T.parens lexer $ T.whiteSpace lexer
      return $ Model.AttrArgFunc name

    argValueFieldReferenceList :: Parser Model.AttrArgValue
    argValueFieldReferenceList =
      Model.AttrArgFieldRefList
        <$> T.brackets lexer (T.commaSep1 lexer $ T.identifier lexer)

    -- NOTE: For now we are not supporting negative numbers.
    --   I couldn't figure out from Prisma docs if there could be the case
    --   where these numbers could be negative.
    --   Same goes for argValueNumberInt below.
    --   TODO: Probably we should take care of that case.
    argValueNumberFloat :: Parser Model.AttrArgValue
    argValueNumberFloat = Model.AttrArgNumber . show <$> T.float lexer

    -- NOTE/TODO: Check comment on argValueNumberFloat.
    argValueNumberInt :: Parser Model.AttrArgValue
    argValueNumberInt = Model.AttrArgNumber . show <$> T.integer lexer

    argValueIdentifier :: Parser Model.AttrArgValue
    argValueIdentifier = Model.AttrArgIdentifier <$> T.identifier lexer

    argValueUnknown :: Parser Model.AttrArgValue
    argValueUnknown =
      Model.AttrArgUnknown <$> many1 (try $ noneOf argDelimiters)

    delimitedArgValue :: Parser Model.AttrArgValue -> Parser Model.AttrArgValue
    delimitedArgValue argValueP = do
      value <- argValueP
      _ <- lookAhead $ oneOf argDelimiters
      return value

    argDelimiters = [',', ')']

blockAttribute :: Parser Model.Attribute
blockAttribute = char '@' >> attribute

lexer :: T.TokenParser ()
lexer =
  T.makeTokenParser
    emptyDef
      { T.commentLine = "//",
        T.caseSensitive = True,
        T.identStart = letter,
        T.identLetter = alphaNum <|> char '_'
      }
