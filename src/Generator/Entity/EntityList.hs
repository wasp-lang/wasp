module Generator.Entity.EntityList
    ( generateEntityList
    , entityListPathInSrc
    ) where

import Control.Exception (assert)
import Data.Aeson ((.=), object, ToJSON(..))
import Data.Maybe (fromJust)
import Path ((</>), reldir, relfile, parseRelFile)
import qualified Path.Aliases as Path

import qualified Wasp
import Wasp (Wasp)
import qualified Wasp.EntityList as WEL

import qualified Generator.FileDraft as FD
import qualified Generator.Entity.Common as EC
import qualified Generator.Common as Common

data EntityListTemplateData = EntityListTemplateData
    { _listName :: !String
    , _entityName :: !String
    , _entityClassName :: !String
    , _entityLowerName :: !String
    , _listFields :: ![ListFieldTemplateData]

    , _entityBeingEditedStateVar :: !String
    }

instance ToJSON EntityListTemplateData where
    toJSON td = object
        [ "listName" .= _listName td
        , "entityName" .= _entityName td
        , "entityClassName" .= _entityClassName td
        , "entityLowerName" .= _entityLowerName td
        , "listFields" .= _listFields td
        , "entityBeingEditedStateVar" .= _entityBeingEditedStateVar td
        ]

data ListFieldTemplateData = ListFieldTemplateData
    { _fieldName :: !String
    , _fieldType :: !Wasp.EntityFieldType
    }

instance ToJSON ListFieldTemplateData where
    toJSON f = EC.addEntityFieldTypeToJsonAsKeyWithValueTrue (_fieldType f) $
        object
            [ "name" .= _fieldName f
            , "type" .= _fieldType f
            ]

createEntityListTemplateData :: Wasp.Entity -> WEL.EntityList -> EntityListTemplateData
createEntityListTemplateData entity entityList =
    assert (Wasp.entityName entity == WEL._entityName entityList) $

        EntityListTemplateData
            { _listName = WEL._name entityList
            , _entityName = Wasp.entityName entity
            , _entityClassName = EC.getEntityClassName entity
            , _entityLowerName = EC.getEntityLowerName entity
            , _listFields = map (createListFieldTD entityList) $ Wasp.entityFields entity
            , _entityBeingEditedStateVar = entityLowerName ++ "BeingEdited"
            }
            where
                entityLowerName = EC.getEntityLowerName entity

createListFieldTD :: WEL.EntityList -> Wasp.EntityField -> ListFieldTemplateData
createListFieldTD _ entityField = ListFieldTemplateData
    { _fieldName = Wasp.entityFieldName entityField
    , _fieldType = Wasp.entityFieldType entityField
    }

generateEntityList :: Wasp -> WEL.EntityList -> FD.FileDraft
generateEntityList wasp entityList =
    FD.createTemplateFileDraft dstPath templateSrcPath (Just templateData)
  where
    -- NOTE(matija): There should always be an entity in wasp for the given entity list.
    -- If not, we want an error to be thrown.
    entity = maybe
        (error $ "Wasp must contain entity to which the entity list refers: " ++
            WEL._entityName entityList)
        id
        (Wasp.getEntityByName wasp (WEL._entityName entityList))

    templateSrcPath = EC.entityTemplatesDirPath </> [reldir|components|] </> [relfile|List.js|]

    dstPath = Common.srcDirPath </> (entityListPathInSrc entity entityList)

    templateData = toJSON $ createEntityListTemplateData entity entityList

-- | Path in the generated src dir where the given entity list will be located.
entityListPathInSrc :: Wasp.Entity -> WEL.EntityList -> Path.RelFile
entityListPathInSrc entity entityList =
    EC.entityComponentsDirPathInSrc entity </>
    (fromJust $ parseRelFile $ (WEL._name entityList) ++ ".js")
