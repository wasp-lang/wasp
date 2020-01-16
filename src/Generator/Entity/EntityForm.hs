module Generator.Entity.EntityForm
    ( generateEntityCreateForm

      -- For testing
    , entityCreateFormPathInSrc
    ) where

import Data.Aeson (toJSON)
--import qualified Data.Aeson as Aeson
import System.FilePath ((</>))

import Wasp
import qualified Util
import Generator.FileDraft
import Generator.Entity.Common
import qualified Generator.Common as Common

-- | Generates entity creation form.
generateEntityCreateForm :: Wasp -> EntityForm -> FileDraft
generateEntityCreateForm wasp entityForm =
    createTemplateFileDraft dstPath templateSrcPath (Just templateData)
  where
    -- NOTE(matija): There should always be an entity in wasp for the given entity form,
    -- we want an error to be thrown otherwise.
    entity = maybe
        (error $ "Wasp must contain entity to which the entity form refers: " ++
         efEntityName entityForm)
        id
        (getEntityByName wasp (efEntityName entityForm))

    templateSrcPath = entityTemplatesDirPath </> "components" </> "CreateForm.js"
    dstPath = Common.srcDirPath </> (entityCreateFormPathInSrc entity entityForm)

    entityTemplateJson = entityTemplateData wasp entity
    templateData = Util.jsonSet "entityForm" (toJSON entityForm) entityTemplateJson

entityCreateFormPathInSrc :: Entity -> EntityForm -> FilePath
entityCreateFormPathInSrc entity entityForm =
    (entityComponentsDirPathInSrc entity) </> (efName entityForm) ++ ".js"
