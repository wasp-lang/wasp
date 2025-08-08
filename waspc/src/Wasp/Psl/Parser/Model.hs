module Wasp.Psl.Parser.Model
  ( parseBody,
    model,
    body,
  )
where

import Control.Arrow (left)
import Data.Maybe (maybeToList)
import Text.Megaparsec (choice, errorBundlePretty, many, optional, some, try)
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Parser.Attribute (attribute, blockAttribute)
import Wasp.Psl.Parser.Common
  ( Parser,
    SourceCode,
    braces,
    identifier,
    parens,
    reserved,
    stringLiteral,
    symbol,
    whiteSpace,
  )
import Wasp.Psl.Parser.WithCtx (withCtx)

-- | This is used to parse the body of the PSL tags in the Wasp file.
-- NOTE: We need to consume the leading whitespace specifically here, because we use the `body`
-- parser directly (meaning not as part of parsing the whole schema) which means that the
-- leading whitespace is not consumed by the `schema` parser.
parseBody :: SourceCode -> Either String Psl.Model.Body
parseBody = left errorBundlePretty . Megaparsec.parse (whiteSpace >> body) ""

-- | Parses PSL (Prisma Schema Language model).
-- Example of PSL model:
--   model User {
--     id Int @id
--     name String
--     @@index([name])
--   }
model :: Parser Psl.Model.Model
model = do
  reserved "model"
  modelName <- identifier
  Psl.Model.Model modelName <$> braces body

-- | Parses body of the PSL (Prisma Schema Language) model,
-- which is everything besides model keyword, name and braces:
--   `model User { <body> }`.
body :: Parser Psl.Model.Body
body = Psl.Model.Body <$> some (withCtx element)

element :: Parser Psl.Model.Element
element =
  choice
    [ try (Psl.Model.ElementField <$> field),
      try (Psl.Model.ElementBlockAttribute <$> blockAttribute)
    ]

field :: Parser Psl.Model.Field
field = do
  name <- identifier
  type' <- fieldType
  maybeTypeModifier <- fieldTypeModifier
  attrs <- many (try attribute)
  return $
    Psl.Model.Field
      { Psl.Model._name = name,
        Psl.Model._type = type',
        Psl.Model._typeModifiers = maybeToList maybeTypeModifier,
        Psl.Model._attrs = attrs
      }
  where
    fieldType :: Parser Psl.Model.FieldType
    fieldType =
      choice
        [ scalarFieldType,
          try
            ( Psl.Model.Unsupported
                <$> ( symbol "Unsupported"
                        >> parens stringLiteral
                    )
            ),
          Psl.Model.UserType <$> identifier
        ]

    scalarFieldType :: Parser Psl.Model.FieldType
    scalarFieldType =
      choice
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
    fieldTypeModifier :: Parser (Maybe Psl.Model.FieldTypeModifier)
    fieldTypeModifier =
      optional $
        choice
          [ try (symbol "[]" >> return Psl.Model.List),
            try (symbol "?" >> return Psl.Model.Optional)
          ]
