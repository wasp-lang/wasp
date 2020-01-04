module Parser.EntityForm
    ( entityForm

    -- For testing
    , entityFormOptionSubmit
    , EntityFormOption (..)

    , submitConfig
    ) where

import Text.Parsec.String (Parser)

import qualified Wasp
import qualified Parser.Common as P
import qualified Util as U
import qualified Lexer as L

-- | Parses entity form, e.g. "entity-form<Task> {...}"
entityForm :: Parser Wasp.EntityForm
entityForm = do
    (entityName, formName, options) <- P.waspElementLinkedToEntity L.reservedNameEntityForm entityFormOptions

    return Wasp.EntityForm
        { Wasp.efName = formName
        , Wasp.efEntityName = entityName
        , Wasp.efSubmitConfig = maybeGetSubmitConfig options
        }

entityFormOptions :: Parser [EntityFormOption]
entityFormOptions = L.commaSep entityFormOption

entityFormOption :: Parser EntityFormOption
entityFormOption = entityFormOptionSubmit
-- TODO(matija): <|> entityFormOptionFields

entityFormOptionSubmit :: Parser EntityFormOption
entityFormOptionSubmit = EfoSubmit <$> (P.waspPropertyClosure "submit" submitConfig)

submitConfig :: Parser Wasp.EntityFormSubmitConfig
submitConfig = do
    onEnter <- P.waspPropertyBool "onEnter"

    return Wasp.EntityFormSubmitConfig
        { Wasp.onEnter = onEnter
        }

maybeGetSubmitConfig :: [EntityFormOption] -> Maybe Wasp.EntityFormSubmitConfig
maybeGetSubmitConfig options = U.headSafe [s | EfoSubmit s <- options]

data EntityFormOption = EfoSubmit Wasp.EntityFormSubmitConfig deriving (Show, Eq)
-- TODO(matija): | EfoFields Wasp.EntityFormFieldsConfig deriving (Show, Eq)
