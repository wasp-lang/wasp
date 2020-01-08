module Parser.EntityForm
    ( entityForm

    -- For testing
    , entityFormOptionSubmit
    , EntityFormOption (..)

    , submitConfig
    ) where

import Text.Parsec.String (Parser)

import qualified Wasp.EntityForm as EntityForm
import qualified Parser.Common as P
import qualified Util as U
import qualified Lexer as L

-- | Parses entity form, e.g. "entity-form<Task> {...}"
entityForm :: Parser EntityForm.EntityForm
entityForm = do
    (entityName, formName, options) <- P.waspElementLinkedToEntity L.reservedNameEntityForm entityFormOptions

    return EntityForm.EntityForm
        { EntityForm.efName = formName
        , EntityForm.efEntityName = entityName
        , EntityForm.efSubmitConfig = maybeGetSubmitConfig options
        }

entityFormOptions :: Parser [EntityFormOption]
entityFormOptions = L.commaSep entityFormOption

entityFormOption :: Parser EntityFormOption
entityFormOption = entityFormOptionSubmit
-- TODO(matija): <|> entityFormOptionFields

entityFormOptionSubmit :: Parser EntityFormOption
entityFormOptionSubmit = EfoSubmit <$> (P.waspPropertyClosure "submit" submitConfig)

submitConfig :: Parser EntityForm.EntityFormSubmitConfig
submitConfig = do
    onEnter <- P.waspPropertyBool "onEnter"

    return EntityForm.EntityFormSubmitConfig
        { EntityForm.onEnter = onEnter
        }

maybeGetSubmitConfig :: [EntityFormOption] -> Maybe EntityForm.EntityFormSubmitConfig
maybeGetSubmitConfig options = U.headSafe [s | EfoSubmit s <- options]

data EntityFormOption = EfoSubmit EntityForm.EntityFormSubmitConfig deriving (Show, Eq)
-- TODO(matija): | EfoFields EntityForm.EntityFormFieldsConfig deriving (Show, Eq)
