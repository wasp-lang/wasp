module Parser.Entity
    ( entity

    -- For testing purposes.
    , entityFieldType
    , entityField
    , entityFields
    ) where

import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import Lexer
    ( identifier
    , colon
    , commaSep1
    , reserved
    , reservedNameEntity
    , reservedNameString
    , reservedNameBoolean
    )
import qualified Wasp
import qualified Parser.Common as P

-- | Top level parser, parses Entity.
entity :: Parser Wasp.Entity
entity = do
    (name, fields) <- P.waspElementNameAndClosure reservedNameEntity entityFields

    return Wasp.Entity
        { Wasp.entityName = name
        , Wasp.entityFields = fields
        }

-- Parses entity fields.
entityFields :: Parser [Wasp.EntityField]
entityFields = commaSep1 entityField

-- | Parses a single entity field, e.g. "title :: string".
entityField :: Parser Wasp.EntityField
entityField = do
    fieldName <- identifier
    _ <- typeAssignmentOp
    fieldType <- entityFieldType

    return $ Wasp.EntityField fieldName fieldType
  where
    -- TODO(matija): maybe specify it under reservedOps in Lexer?
    typeAssignmentOp = colon *> colon

-- | Parses one of the supported types of the entity field e.g. "string" or "boolean".
entityFieldType :: Parser Wasp.EntityFieldType
entityFieldType = entityFieldTypeString <|> entityFieldTypeBoolean

-- | Parses string type of entity field.
entityFieldTypeString :: Parser Wasp.EntityFieldType
entityFieldTypeString = reserved reservedNameString *> pure Wasp.EftString

-- | Parses boolean type of entity field.
entityFieldTypeBoolean :: Parser Wasp.EntityFieldType
entityFieldTypeBoolean = reserved reservedNameBoolean *> pure Wasp.EftBoolean
