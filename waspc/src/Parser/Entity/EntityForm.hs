module Parser.Entity.EntityForm
    ( entityForm

    -- For testing
    , entityFormOptionSubmit
    , EntityFormOption (..)

    , submitConfig
    ) where

import Text.Parsec (choice, (<|>))
import Text.Parsec.String (Parser)

import qualified Wasp.EntityForm as WEF
import Wasp.EntityForm (EntityForm)

import qualified Parser.Common as P
import qualified Parser.Entity.Common as PE
import qualified Util as U
import qualified Lexer as L


-- * EntityForm

-- | Parses entity form, e.g. "entity-form<Task> NewTaskForm {...}"
entityForm :: Parser EntityForm
entityForm = do
    (entityName, formName, options) <-
        P.waspElementLinkedToEntity L.reservedNameEntityForm (P.waspClosure entityFormOptions)

    return WEF.EntityForm
        { WEF._name = formName
        , WEF._entityName = entityName
        , WEF._submit = maybeGetSubmitConfig options
        , WEF._fields = getFieldsConfig options
        }

data EntityFormOption 
    = EfoSubmit WEF.Submit
    | EfoFields [WEF.Field]
    deriving (Show, Eq)

entityFormOptions :: Parser [EntityFormOption]
entityFormOptions = L.commaSep entityFormOption

entityFormOption :: Parser EntityFormOption
entityFormOption = choice
    [ entityFormOptionSubmit
    , entityFormOptionFields
    ]

-- * Submit

maybeGetSubmitConfig :: [EntityFormOption] -> Maybe WEF.Submit
maybeGetSubmitConfig options = U.headSafe [s | EfoSubmit s <- options]

entityFormOptionSubmit :: Parser EntityFormOption
entityFormOptionSubmit = EfoSubmit <$> (P.waspPropertyClosure "submit" submitConfig)

submitConfig :: Parser WEF.Submit
submitConfig = do
    -- TODO(matija): this pattern of "having at least 1 property in closure" could be further
    -- extracted to e.g. "waspClosureOptions" - but again sometimes it is ok not to have any props,
    -- e.g. EntityForm. Maybe then "waspClosureOptions1" and "waspClosureOptions"?
    options <- L.commaSep1 submitOption

    return WEF.Submit
        { WEF._onEnter = maybeGetSoOnEnter options
        , WEF._submitButton = maybeGetSoSubmitButton options
        }

data SubmitOption = SoOnEnter Bool | SoSubmitButton WEF.SubmitButton deriving (Show, Eq)

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

maybeGetSoSubmitButton :: [SubmitOption] -> Maybe WEF.SubmitButton
maybeGetSoSubmitButton options = U.headSafe [sb | SoSubmitButton sb <- options]

submitButtonConfig :: Parser WEF.SubmitButton
submitButtonConfig = do
    options <- L.commaSep1 submitButtonOption

    return WEF.SubmitButton
        { WEF._submitButtonShow = maybeGetSboShow options
        }

data SubmitButtonOption = SboShow Bool deriving (Show, Eq)

submitButtonOption :: Parser SubmitButtonOption
submitButtonOption = submitButtonOptionShow -- <|> anotherOption <|> ...

submitButtonOptionShow :: Parser SubmitButtonOption
submitButtonOptionShow = SboShow <$> P.waspPropertyBool "show"

maybeGetSboShow :: [SubmitButtonOption] -> Maybe Bool
maybeGetSboShow options = U.headSafe [b | SboShow b <- options]

-- * Fields

getFieldsConfig :: [EntityFormOption] -> [WEF.Field]
getFieldsConfig options = case [fs | EfoFields fs <- options] of
    [] -> []
    ls -> head ls

entityFormOptionFields :: Parser EntityFormOption
entityFormOptionFields = EfoFields <$> PE.waspPropertyEntityFields fieldOption createFieldConfig

createFieldConfig :: (String, [FieldOption]) -> WEF.Field
createFieldConfig (fieldName, options) = WEF.Field
    { WEF._fieldName = fieldName
    , WEF._fieldShow = maybeGetFieldOptionShow options
    , WEF._fieldDefaultValue = maybeGetFieldOptionDefaultValue options
    , WEF._fieldPlaceholder = maybeGetFieldOptionPlaceholder options
    , WEF._fieldLabel = maybeGetFieldOptionLabel options
    }
    
data FieldOption
    = FieldOptionShow Bool
    | FieldOptionDefaultValue WEF.DefaultValue
    | FieldOptionPlaceholder String
    | FieldOptionLabel (Maybe String)
    deriving (Show, Eq)

-- | Parses a single field option, e.g. "show" or "defaultValue".
fieldOption :: Parser FieldOption
fieldOption = choice
    [ FieldOptionShow <$> P.waspPropertyBool "show"
    , FieldOptionDefaultValue <$> defaultValue
    , FieldOptionPlaceholder <$> P.waspPropertyStringLiteral "placeholder"
    , FieldOptionLabel <$> fieldOptionLabel
    ]

defaultValue :: Parser WEF.DefaultValue
defaultValue = P.waspProperty "defaultValue" $ choice
    [ WEF.DefaultValueString <$> L.stringLiteral
    , WEF.DefaultValueBool <$> L.bool
    ]

fieldOptionLabel :: Parser (Maybe String)
fieldOptionLabel = P.waspProperty "label" labelValue
    where
        labelValue = (Just <$> L.stringLiteral) <|> (L.symbol "none" *> pure Nothing)

maybeGetFieldOptionShow :: [FieldOption] -> Maybe Bool
maybeGetFieldOptionShow options = U.headSafe [b | FieldOptionShow b <- options]

maybeGetFieldOptionDefaultValue :: [FieldOption] -> Maybe WEF.DefaultValue
maybeGetFieldOptionDefaultValue options  = U.headSafe [dv | FieldOptionDefaultValue dv <- options]

maybeGetFieldOptionPlaceholder :: [FieldOption] -> Maybe String
maybeGetFieldOptionPlaceholder options = U.headSafe [s | FieldOptionPlaceholder s <- options]

maybeGetFieldOptionLabel :: [FieldOption] -> Maybe (Maybe String)
maybeGetFieldOptionLabel options = U.headSafe [ms | FieldOptionLabel ms <- options]
