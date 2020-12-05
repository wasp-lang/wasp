module Psl.Parser.Model
    ( model
    , body
    -- NOTE: Only for testing:
    , attrArgument
    ) where

import           Data.Maybe           (fromMaybe, maybeToList)
import           Text.Parsec          (alphaNum, char, choice, letter,
                                       lookAhead, many, many1, noneOf, oneOf,
                                       optionMaybe, try, (<|>))
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as T

import qualified Psl.Ast.Model        as Model

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
element = try (Model.ElementField <$> field) <|>
          try (Model.ElementBlockAttribute <$> blockAttribute)

field :: Parser Model.Field
field = do
    name <- T.identifier lexer
    type' <- fieldType
    maybeTypeModifier <- fieldTypeModifier
    attrs <- many (try attribute)
    return $ Model.Field
        { Model._name = name
        , Model._type = type'
        , Model._typeModifiers = maybeToList maybeTypeModifier
        , Model._attrs = attrs
        }
  where
    fieldType :: Parser Model.FieldType
    fieldType =
        (foldl1 (<|>) $
         map (\(s, t) -> try (T.symbol lexer s) >> return t)
         [ ("String",   Model.String)
         , ("Boolean",  Model.Boolean)
         , ("Int",      Model.Int)
         , ("Float",    Model.Float)
         , ("DateTime", Model.DateTime)
         , ("Json",     Model.Json)
         ]
        )
        <|> Model.UserType <$> T.identifier lexer

    -- NOTE: As is Prisma currently implemented, there can be only one type modifier at one time: [] or ?.
    fieldTypeModifier :: Parser (Maybe Model.FieldTypeModifier)
    fieldTypeModifier = optionMaybe
        ( (try (T.brackets lexer (T.whiteSpace lexer)) >> return Model.List) <|>
          (try (T.symbol lexer "?") >> return Model.Optional)
        )

attribute :: Parser Model.Attribute
attribute = do
    _ <- char '@'
    name <- T.identifier lexer
    maybeArgs <- optionMaybe (T.parens lexer (T.commaSep1 lexer (try attrArgument)))
    return $ Model.Attribute
        { Model._attrName = name
        , Model._attrArgs = fromMaybe [] maybeArgs
        }

-- Parses attribute argument that ends with delimiter: , or ).
-- Doesn't parse the delimiter.
attrArgument :: Parser Model.AttributeArg
attrArgument = do
    arg <- try namedArg <|> try unnamedArg
    _ <- try $ lookAhead $ oneOf argDelimiters
    return arg
  where
    namedArg :: Parser Model.AttributeArg
    namedArg = do
        name <- T.identifier lexer
        _ <- T.colon lexer
        Model.AttrArgNamed name <$> argValue

    unnamedArg :: Parser Model.AttributeArg
    unnamedArg = Model.AttrArgUnnamed <$> argValue

    argValue :: Parser Model.AttrArgValue
    argValue = choice $ map (try . delimitedArgValue)
        [ argValueString
        , argValueFunc
        , argValueFieldReferenceList
        , argValueNumberFloat
        , argValueNumberInt
        , argValueIdentifier
        , argValueUnknown
        ]

    argValueString :: Parser Model.AttrArgValue
    argValueString = Model.AttrArgString <$> T.stringLiteral lexer

    argValueFunc :: Parser Model.AttrArgValue
    argValueFunc = do -- TODO: Could I implement this with applicative?
        name <- T.identifier lexer
        T.parens lexer $ T.whiteSpace lexer
        return $ Model.AttrArgFunc name

    argValueFieldReferenceList :: Parser Model.AttrArgValue
    argValueFieldReferenceList = Model.AttrArgFieldRefList <$>
        (T.brackets lexer $ T.commaSep1 lexer $ T.identifier lexer)

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

    -- | Our "wildcard" -> tries to capture anything.
    argValueUnknown :: Parser Model.AttrArgValue
    argValueUnknown = Model.AttrArgUnknown <$>
        (many1 $ try $ noneOf argDelimiters)

    delimitedArgValue :: Parser Model.AttrArgValue -> Parser Model.AttrArgValue
    delimitedArgValue argValueP = do
        value <- argValueP
        _ <- lookAhead $ oneOf argDelimiters
        return value

    argDelimiters = [',', ')']

blockAttribute :: Parser Model.Attribute
blockAttribute = char '@' >> attribute

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef
    { T.commentLine = "//"
    , T.caseSensitive = True
    , T.identStart = letter
    , T.identLetter = alphaNum <|> char '_'
    }
