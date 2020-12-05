module PslModelAst where

data Model = Model
    String -- ^ Name of the model
    Body
    deriving (Show, Eq)

newtype Body = Body [Element]
    deriving (Show, Eq)

data Element = ElementField Field | ElementBlockAttribute Attribute
    deriving (Show, Eq)

-- TODO: To support attributes before the field,
--   we could just have `attrsBefore :: [[Attr]]`,
--   which represents lines, each one with list of attributes.
data Field = Field
    { _name :: String
    , _type :: FieldType
    , _typeModifiers :: [FieldTypeModifier]
    , _attrs :: [Attribute]
    }
    deriving (Show, Eq)

data FieldType = String | Boolean | Int | Float | DateTime | Json | UserType String
    deriving (Show, Eq)

data FieldTypeModifier = List | Optional
    deriving (Show, Eq)

data Attribute = Attribute
    { _attrName :: String
    , _attrArgs :: [AttributeArg]
    }
    deriving (Show, Eq)

data AttributeArg = AttrArgNamed String AttrArgValue | AttrArgUnnamed AttrArgValue
    deriving (Show, Eq)

data AttrArgValue
    = AttrArgString String
    | AttrArgIdentifier String
    | AttrArgFunc String
    | AttrArgFieldRefList [String]
    | AttrArgNumber String
    | AttrArgUnknown String
    deriving (Show, Eq)
