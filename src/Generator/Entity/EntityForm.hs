module Generator.Entity.EntityForm
    ( generateEntityCreateForm

      -- For testing
    , entityCreateFormPathInSrc
    ) where

import Data.Aeson ((.=), object, ToJSON(..))
import qualified Data.Aeson as Aeson
import Control.Exception (assert)
import Data.Maybe (fromJust)
import Path ((</>), reldir, relfile)
import qualified Path
import qualified Path.Aliases as Path

import Wasp
import qualified Wasp.EntityForm as EF
import Wasp.EntityForm (EntityForm)

import Generator.FileDraft
import qualified Generator.Entity.Common as EC
import qualified Generator.Common as Common

data TemplateData = TemplateData
    { _name :: !String
    , _entityClassName :: !String
    , _entityFields :: ![EntityField]
    -- Submit
    , _showSubmitButton :: !Bool
    } deriving (Show)

instance ToJSON TemplateData where
    toJSON td = object
        [ "name" .= _name td
        , "entityClassName" .= _entityClassName td
        , "entityTypedFields" .= map EC.entityFieldToJsonWithTypeAsKey (_entityFields td)
        , "showSubmitButton" .= _showSubmitButton td
        ]

createEntityFormTemplateData :: Entity -> EntityForm -> Aeson.Value
createEntityFormTemplateData entity entityForm =
    assert (entityName entity == EF._entityName entityForm) $

    toJSON $ TemplateData
    { _name = EF._name entityForm
    , _entityClassName = EC.getEntityClassName entity
    , _entityFields = entityFields entity
    -- Submit
    , _showSubmitButton = maybe True id maybeShowSubmitButton
    }
    where
        maybeShowSubmitButton :: Maybe Bool
        maybeShowSubmitButton = EF._submit entityForm >>= EF._submitButton >>= EF._show

-- | Generates entity creation form.
generateEntityCreateForm :: Wasp -> EntityForm -> FileDraft
generateEntityCreateForm wasp entityForm =
    createTemplateFileDraft dstPath templateSrcPath (Just templateData)
  where
    -- NOTE(matija): There should always be an entity in wasp for the given entity form,
    -- we want an error to be thrown otherwise.
    entity = maybe
        (error $ "Wasp must contain entity to which the entity form refers: " ++
         EF._entityName entityForm)
        id
        (getEntityByName wasp (EF._entityName entityForm))

    templateSrcPath = EC.entityTemplatesDirPath </> [reldir|components|] </> [relfile|CreateForm.js|]

    dstPath = Common.srcDirPath </> (entityCreateFormPathInSrc entity entityForm)

    templateData = createEntityFormTemplateData entity entityForm

entityCreateFormPathInSrc :: Entity -> EntityForm -> Path.RelFile
entityCreateFormPathInSrc entity entityForm =
    EC.entityComponentsDirPathInSrc entity </>
    (fromJust $ Path.parseRelFile $ (EF._name entityForm) ++ ".js")
