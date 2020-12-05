module Parser.PslModel
    ( model
    , body
    -- NOTE: Only for testing:
    , attrArgument
    ) where

import Data.Maybe (fromMaybe, maybeToList)
import Text.Parsec ((<|>), many, letter, alphaNum, char, try, optionMaybe, many1, noneOf)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

import qualified PslModelAst as AST

-- | Parses PSL (Prisma Schema Language model).
-- Example of PSL model:
--   model User {
--     id Int @id
--     name String
--     @@index([name])
--   }
model :: Parser AST.Model
model = do
    T.whiteSpace lexer
    _ <- T.symbol lexer "model"
    modelName <- T.identifier lexer
    AST.Model modelName <$> T.braces lexer body

-- | Parses body of the PSL (Prisma Schema Language) model,
-- which is everything besides model keyword, name and braces:
--   `model User { <body> }`.
body :: Parser AST.Body
body = do
    T.whiteSpace lexer
    AST.Body <$> many1 element

element :: Parser AST.Element
element = try (AST.ElementField <$> field) <|>
          try (AST.ElementBlockAttribute <$> blockAttribute)

field :: Parser AST.Field
field = do
    name <- T.identifier lexer
    type' <- fieldType
    maybeTypeModifier <- fieldTypeModifier
    attrs <- many (try attribute)
    return $ AST.Field
        { AST._name = name
        , AST._type = type'
        , AST._typeModifiers = maybeToList maybeTypeModifier
        , AST._attrs = attrs
        }
  where
    fieldType :: Parser AST.FieldType
    fieldType =
        (foldl1 (<|>) $
         map (\(s, t) -> try (T.symbol lexer s) >> return t)
         [ ("String", AST.String)
         , ("Boolean", AST.Boolean)
         , ("Int", AST.Int)
         , ("Float", AST.Float)
         , ("DateTime", AST.DateTime)
         , ("Json", AST.Json)
         ]
        )
        <|> AST.UserType <$> T.identifier lexer

    -- NOTE: As is Prisma currently implemented, there can be only one type modifier at one time: [] or ?.
    fieldTypeModifier :: Parser (Maybe AST.FieldTypeModifier)
    fieldTypeModifier = optionMaybe
        ( (try (T.brackets lexer (T.whiteSpace lexer)) >> return AST.List) <|>
          (try (T.symbol lexer "?") >> return AST.Optional)
        )

attribute :: Parser AST.Attribute
attribute = do
    _ <- char '@'
    name <- T.identifier lexer
    maybeArgs <- optionMaybe (T.parens lexer (T.commaSep1 lexer attrArgument))
    return $ AST.Attribute
        { AST._attrName = name
        , AST._attrArgs = fromMaybe [] maybeArgs
        }

attrArgument :: Parser AST.AttributeArg
attrArgument = try namedArg <|> try unnamedArg
  where
    namedArg :: Parser AST.AttributeArg
    namedArg = do
        name <- T.identifier lexer
        _ <- T.colon lexer
        AST.AttrArgNamed name <$> argValue

    unnamedArg :: Parser AST.AttributeArg
    unnamedArg = AST.AttrArgUnnamed <$> argValue

    argValue :: Parser AST.AttrArgValue
    argValue =
        try argValueString <|>
        try argValueFunc <|>
        try argValueFieldReferenceList <|>
        try argValueNumber <|>
        try argValueIdentifier <|>
        try argValueUnknown

    argValueString :: Parser AST.AttrArgValue
    argValueString = AST.AttrArgString <$> T.stringLiteral lexer

    argValueFunc :: Parser AST.AttrArgValue
    argValueFunc = do -- TODO: Could I implement this with applicative?
        name <- T.identifier lexer
        T.parens lexer $ T.whiteSpace lexer
        return $ AST.AttrArgFunc name

    argValueFieldReferenceList :: Parser AST.AttrArgValue
    argValueFieldReferenceList = AST.AttrArgFieldRefList <$>
        (T.brackets lexer $ T.commaSep1 lexer $ T.identifier lexer)

    argValueNumber :: Parser AST.AttrArgValue
    argValueNumber = AST.AttrArgNumber . show <$> T.float lexer

    argValueIdentifier :: Parser AST.AttrArgValue
    argValueIdentifier = AST.AttrArgIdentifier <$> T.identifier lexer

    -- | Our "wildcard" -> tries to capture anything.
    argValueUnknown :: Parser AST.AttrArgValue
    argValueUnknown = AST.AttrArgUnknown <$>
        (many1 $ try $ noneOf argDelimiters)
      where argDelimiters = [',', ')']

blockAttribute :: Parser AST.Attribute
blockAttribute = char '@' >> attribute

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef
    { T.commentLine = "//"
    , T.caseSensitive = True
    , T.identStart = letter
    , T.identLetter = alphaNum <|> char '_'
    }
