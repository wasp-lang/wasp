module Parser.Entity
    ( entity
    ) where

import           Text.Parsec.String (Parser)

import qualified Lexer              as L
import qualified Psl.Ast.Model      as PslModel
import qualified Psl.Parser.Model
import qualified Wasp.Entity        as Entity

entity :: Parser Entity.Entity
entity = do
    _ <- L.reserved L.reservedNameEntity
    name <- L.identifier
    _ <- L.symbol "{=psl"
    pslModelBody <- Psl.Parser.Model.body
    _ <- L.symbol "psl=}"

    return Entity.Entity
        { Entity._name = name
        , Entity._fields = getEntityFields pslModelBody
        , Entity._pslModelBody = pslModelBody
        }

getEntityFields :: PslModel.Body -> [Entity.Field]
getEntityFields (PslModel.Body pslElements) = map pslFieldToEntityField pslFields
  where
    pslFields = [field | (PslModel.ElementField field) <- pslElements]

    pslFieldToEntityField :: PslModel.Field -> Entity.Field
    pslFieldToEntityField pslField = Entity.Field
        { Entity._fieldName = PslModel._name pslField
        , Entity._fieldType = pslFieldTypeToEntityFieldType
                    (PslModel._type pslField)
                    (PslModel._typeModifiers pslField)
        }

    pslFieldTypeToEntityFieldType
        :: PslModel.FieldType
        -> [PslModel.FieldTypeModifier]
        -> Entity.FieldType
    pslFieldTypeToEntityFieldType fType fTypeModifiers =
        let scalar = pslFieldTypeToScalar fType
        in case fTypeModifiers of
               []               -> Entity.FieldTypeScalar scalar
               [PslModel.List]     -> Entity.FieldTypeComposite $ Entity.List scalar
               [PslModel.Optional] -> Entity.FieldTypeComposite $ Entity.Optional scalar
               _                -> error "Not a valid list of modifiers."

    pslFieldTypeToScalar :: PslModel.FieldType -> Entity.Scalar
    pslFieldTypeToScalar fType = case fType of
        PslModel.String               -> Entity.String
        PslModel.Boolean              -> Entity.Boolean
        PslModel.Int                  -> Entity.Int
        PslModel.BigInt               -> Entity.BigInt
        PslModel.Float                -> Entity.Float
        PslModel.Decimal              -> Entity.Decimal
        PslModel.DateTime             -> Entity.DateTime
        PslModel.Json                 -> Entity.Json
        PslModel.Bytes                -> Entity.Bytes
        PslModel.UserType typeName    -> Entity.UserType typeName
        PslModel.Unsupported typeName -> Entity.Unsupported typeName
