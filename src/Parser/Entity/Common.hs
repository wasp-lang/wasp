module Parser.Entity.Common
    ( waspPropertyEntityFields
    ) where

import Text.Parsec.String (Parser)

import qualified Lexer as L
import Parser.Common as P

-- A function that takes an entity field name (e.g. "description) and a list of parsed field
-- options, and then creates a final Wasp AST record from it (fieldConfig).
type CreateFieldConfig fieldOption fieldConfig = (String, [fieldOption]) -> fieldConfig

-- | Parses configuration of fields within a wasp entity component (e.g. entity-form
-- or entity-list). Parses the following format:
--
-- fields: { FIELD_NAME: {...}, FIELD_NAME: {...}, ... }
--
-- At least one field must be specified.
waspPropertyEntityFields
    :: Parser fo                -- ^ Parser of a single field option.
    -> CreateFieldConfig fo fc  -- ^ Function that creates a record with all parsed field options.
    -> Parser [fc]              -- ^ Field configs, a list of record with all the field options.
waspPropertyEntityFields fieldOptionP createFieldConfig = P.waspPropertyClosure "fields" $
    L.commaSep1 $ waspPropertyEntityField fieldOptionP createFieldConfig


-- | Parses configuration of a specific field within a wasp entity component (e.g. entity-form
-- or entity-list). Parses the following format:
--
-- FIELD_NAME: { option1, option2 }
--
-- At least one option must be present.
waspPropertyEntityField
    :: Parser fo                -- ^ Parser of a single field option.
    -> CreateFieldConfig fo fc  -- ^ Function that creates a record with all parsed field options.
    -> Parser fc                -- ^ Field config, a record with all the field options.
waspPropertyEntityField fieldOptionP createFieldConfig =
    (P.waspIdentifierClosure $ L.commaSep1 fieldOptionP) >>= (return . createFieldConfig)
