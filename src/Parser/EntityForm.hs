module Parser.EntityForm
    ( entityForm

    -- For testing
    , entityFormOptionSubmit
    , EntityFormOption (..)

    , submitConfig
    ) where

import Text.Parsec (choice)
import Text.Parsec.String (Parser)

import qualified Wasp.EntityForm as EF
import Wasp.EntityForm (EntityForm)

import qualified Parser.Common as P
import qualified Util as U
import qualified Lexer as L


-- * EntityForm

-- | Parses entity form, e.g. "entity-form<Task> NewTaskForm {...}"
entityForm :: Parser EntityForm
entityForm = do
    (entityName, formName, options) <- P.waspElementLinkedToEntity L.reservedNameEntityForm entityFormOptions

    return EF.EntityForm
        { EF._name = formName
        , EF._entityName = entityName
        , EF._submit = maybeGetSubmitConfig options
        , EF._fields = getFieldsConfig options
        }

data EntityFormOption 
    = EfoSubmit EF.Submit
    | EfoFields [EF.Field]
    deriving (Show, Eq)

entityFormOptions :: Parser [EntityFormOption]
entityFormOptions = L.commaSep entityFormOption

entityFormOption :: Parser EntityFormOption
entityFormOption = choice
    [ entityFormOptionSubmit
    , entityFormOptionFields
    ]

-- * Submit

maybeGetSubmitConfig :: [EntityFormOption] -> Maybe EF.Submit
maybeGetSubmitConfig options = U.headSafe [s | EfoSubmit s <- options]

entityFormOptionSubmit :: Parser EntityFormOption
entityFormOptionSubmit = EfoSubmit <$> (P.waspPropertyClosure "submit" submitConfig)

submitConfig :: Parser EF.Submit
submitConfig = do
    -- TODO(matija): this pattern of "having at least 1 property in closure" could be further
    -- extracted to e.g. "waspClosureOptions" - but again sometimes it is ok not to have any props,
    -- e.g. EntityForm. Maybe then "waspClosureOptions1" and "waspClosureOptions"?
    options <- L.commaSep1 submitOption

    return EF.Submit
        { EF._onEnter = maybeGetSoOnEnter options
        , EF._submitButton = maybeGetSoSubmitButton options
        }

data SubmitOption = SoOnEnter Bool | SoSubmitButton EF.SubmitButton deriving (Show, Eq)

submitOption :: Parser SubmitOption
submitOption = choice [submitOptionOnEnter, submitOptionSubmitButton]

-- onEnter
submitOptionOnEnter :: Parser SubmitOption
submitOptionOnEnter = SoOnEnter <$> P.waspPropertyBool "onEnter"

maybeGetSoOnEnter :: [SubmitOption] -> Maybe Bool
maybeGetSoOnEnter options = U.headSafe [b | SoOnEnter b <- options]

-- submit button
submitOptionSubmitButton :: Parser SubmitOption
submitOptionSubmitButton = SoSubmitButton <$> P.waspPropertyClosure "button" submitButtonConfig

maybeGetSoSubmitButton :: [SubmitOption] -> Maybe EF.SubmitButton
maybeGetSoSubmitButton options = U.headSafe [sb | SoSubmitButton sb <- options]

submitButtonConfig :: Parser EF.SubmitButton
submitButtonConfig = do
    options <- L.commaSep1 submitButtonOption

    return EF.SubmitButton
        { EF._submitButtonShow = maybeGetSboShow options
        }

data SubmitButtonOption = SboShow Bool deriving (Show, Eq)

submitButtonOption :: Parser SubmitButtonOption
submitButtonOption = submitButtonOptionShow -- <|> anotherOption <|> ...

submitButtonOptionShow :: Parser SubmitButtonOption
submitButtonOptionShow = SboShow <$> P.waspPropertyBool "show"

maybeGetSboShow :: [SubmitButtonOption] -> Maybe Bool
maybeGetSboShow options = U.headSafe [b | SboShow b <- options]

-- * Fields

getFieldsConfig :: [EntityFormOption] -> [EF.Field]
getFieldsConfig options = case [fs | EfoFields fs <- options] of
    [] -> []
    ls -> head ls

entityFormOptionFields :: Parser EntityFormOption
entityFormOptionFields = EfoFields <$> (P.waspPropertyClosure "fields" $ L.commaSep1 field)

-- | Parses 'FIELD_NAME: { ... }.'
field :: Parser EF.Field
field = do
    (fieldName, options) <- P.waspIdentifierClosure $ L.commaSep1 fieldOption

    return EF.Field
        { EF._fieldName = fieldName
        , EF._fieldShow = maybeGetFieldOptionShow options
        , EF._fieldDefaultValue = maybeGetFieldOptionDefaultValue options
        }
 
data FieldOption
    = FieldOptionShow Bool
    | FieldOptionDefaultValue EF.DefaultValue
    deriving (Show, Eq)

-- | Parses a single field option, e.g. "show" or "defaultValue".
fieldOption :: Parser FieldOption
fieldOption = choice
    [ FieldOptionShow <$> P.waspPropertyBool "show"
    , FieldOptionDefaultValue <$> defaultValue
    ]

defaultValue :: Parser EF.DefaultValue
defaultValue = P.waspProperty "defaultValue" $ choice
    [ EF.DefaultValueString <$> L.stringLiteral
    , EF.DefaultValueBool <$> L.bool
    ]

maybeGetFieldOptionShow :: [FieldOption] -> Maybe Bool
maybeGetFieldOptionShow options = U.headSafe [b | FieldOptionShow b <- options]

maybeGetFieldOptionDefaultValue :: [FieldOption] -> Maybe EF.DefaultValue
maybeGetFieldOptionDefaultValue options  = U.headSafe [dv | FieldOptionDefaultValue dv <- options]
