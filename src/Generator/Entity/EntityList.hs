module Generator.Entity.EntityList
    ( generateEntityList
    , entityListPathInSrc
    ) where

import Control.Exception (assert)
import Data.Aeson ((.=), object, ToJSON(..))
import Data.Maybe (fromJust)
import Path ((</>), reldir, relfile, parseRelFile)
import qualified Path.Aliases as Path

import qualified Util as U

import qualified Wasp
import Wasp (Wasp)
import qualified Wasp.EntityList as WEL
import qualified Wasp.JsCode

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
    , _fieldWidthAsPercent :: !Int
    -- Render
    , _fieldRender :: Maybe Wasp.JsCode.JsCode
    , _fieldRenderFnName :: String
    }

instance ToJSON ListFieldTemplateData where
    toJSON f = EC.addEntityFieldTypeToJsonAsKeyWithValueTrue (_fieldType f) $
        object
            [ "name" .= _fieldName f
            , "type" .= _fieldType f
            , "widthAsPercent" .= _fieldWidthAsPercent f
            , "render" .= _fieldRender f
            , "renderFnName" .= _fieldRenderFnName f
            ]

createEntityListTemplateData :: Wasp.Entity -> WEL.EntityList -> EntityListTemplateData
createEntityListTemplateData entity entityList =
    assert (Wasp.entityName entity == WEL._entityName entityList) $

        EntityListTemplateData
            { _listName = WEL._name entityList
            , _entityName = Wasp.entityName entity
            , _entityClassName = EC.getEntityClassName entity
            , _entityLowerName = EC.getEntityLowerName entity
            , _listFields = map (createListFieldTD entity entityList) $ Wasp.entityFields entity
            , _entityBeingEditedStateVar = entityLowerName ++ "BeingEdited"
            }
            where
                entityLowerName = EC.getEntityLowerName entity

createListFieldTD :: Wasp.Entity -> WEL.EntityList -> Wasp.EntityField -> ListFieldTemplateData
createListFieldTD entity entityList entityField = ListFieldTemplateData
    { _fieldName = Wasp.entityFieldName entityField
    , _fieldType = Wasp.entityFieldType entityField
    , _fieldWidthAsPercent = fieldWidthAsPercent
    , _fieldRender = listFieldConfig >>= WEL._fieldRender
    , _fieldRenderFnName = "render" ++ entityUpper ++ entityFieldUpper
    }
    where
        -- Configuration of a form field within entity-list, if there is any.
        listFieldConfig :: Maybe WEL.Field
        listFieldConfig = WEL.getConfigForField entityList entityField

        entityUpper = U.toUpperFirst $ Wasp.entityName entity
        entityFieldUpper = U.toUpperFirst $ Wasp.entityFieldName entityField

        -- NOTE(matija): for now we decide on width, it is not yet an option exposed to the
        -- user. Our current strategy, since we have no outside info, is to make all fields
        -- have approx. equal width.
        --
        -- Since we use Int, obviously we will not be able to always get a sum of 100. But with
        -- the current style framework we are using (material-ui), it is not a problem.
        -- 
        -- E.g. if we have 3 fields, each of them will get assigned a width of 33 - material-ui
        -- will then resolve that by itself - it will assign the missing width to one of the
        -- fields (the last one, from what I tried).
        fieldWidthAsPercent :: Int
        fieldWidthAsPercent = 100 `div` (length $ Wasp.entityFields entity)

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
