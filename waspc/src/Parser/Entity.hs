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
import qualified Wasp.Entity as Entity
import qualified Parser.Common as P

-- | Top level parser, parses Entity.
entity :: Parser Entity.Entity
entity = do
    (name, fields) <- P.waspElementNameAndClosureContent reservedNameEntity entityFields

    return Entity.Entity
        { Entity.entityName = name
        , Entity.entityFields = fields
        }

-- Parses entity fields.
entityFields :: Parser [Entity.EntityField]
entityFields = commaSep1 entityField

-- | Parses a single entity field, e.g. "title :: string".
entityField :: Parser Entity.EntityField
entityField = do
    fieldName <- identifier
    _ <- typeAssignmentOp
    fieldType <- entityFieldType

    return $ Entity.EntityField fieldName fieldType
  where
    -- TODO(matija): maybe specify it under reservedOps in Lexer?
    typeAssignmentOp = colon *> colon

-- | Parses one of the supported types of the entity field e.g. "string" or "boolean".
entityFieldType :: Parser Entity.EntityFieldType
entityFieldType = entityFieldTypeString <|> entityFieldTypeBoolean

-- | Parses string type of entity field.
entityFieldTypeString :: Parser Entity.EntityFieldType
entityFieldTypeString = reserved reservedNameString *> pure Entity.EftString

-- | Parses boolean type of entity field.
entityFieldTypeBoolean :: Parser Entity.EntityFieldType
entityFieldTypeBoolean = reserved reservedNameBoolean *> pure Entity.EftBoolean
