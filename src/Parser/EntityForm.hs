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
        }

data EntityFormOption = EfoSubmit EF.Submit deriving (Show, Eq)
-- TODO(matija): | EfoFields EntityForm.EntityFormFieldsConfig deriving (Show, Eq)

entityFormOptions :: Parser [EntityFormOption]
entityFormOptions = L.commaSep entityFormOption

entityFormOption :: Parser EntityFormOption
entityFormOption = entityFormOptionSubmit
-- TODO(matija): <|> entityFormOptionFields

entityFormOptionSubmit :: Parser EntityFormOption
entityFormOptionSubmit = EfoSubmit <$> (P.waspPropertyClosure "submit" submitConfig)

maybeGetSubmitConfig :: [EntityFormOption] -> Maybe EF.Submit
maybeGetSubmitConfig options = U.headSafe [s | EfoSubmit s <- options]


-- * Submit

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
        { EF._show = maybeGetSboShow options
        }

data SubmitButtonOption = SboShow Bool deriving (Show, Eq)

submitButtonOption :: Parser SubmitButtonOption
submitButtonOption = submitButtonOptionShow -- <|> anotherOption <|> ...

submitButtonOptionShow :: Parser SubmitButtonOption
submitButtonOptionShow = SboShow <$> P.waspPropertyBool "show"

maybeGetSboShow :: [SubmitButtonOption] -> Maybe Bool
maybeGetSboShow options = U.headSafe [b | SboShow b <- options]
