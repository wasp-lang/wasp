module Generator.WebAppGenerator.EntityGenerator.EntityListGenerator
    ( generateEntityList
    , entityListPathInSrc
    ) where

import Control.Exception (assert)
import Data.Aeson ((.=), object, ToJSON(..), toJSON)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import qualified Path as P

import StrongPath (Path, Rel, File, (</>))
import qualified StrongPath as SP
import qualified Util as U
import qualified Wasp
import Wasp (Wasp)
import qualified Wasp.EntityList as WEL
import qualified Wasp.JsCode
import qualified Generator.FileDraft as FD
import qualified Generator.WebAppGenerator.EntityGenerator.Common as EC
import qualified Generator.WebAppGenerator.Common as Common


-- * List template data

data EntityListTemplateData = EntityListTemplateData
    { _listName :: !String
    , _entityName :: !String
    , _entityClassName :: !String
    , _entityLowerName :: !String
    , _listShowHeader :: !Bool
    , _listFields :: ![ListFieldTemplateData]
    -- NOTE(matija): Although this field might be Nothing, it won't
    -- work for the inverted section because the value of the field
    -- will be null (and it needs to be false or an empty list).
    --
    -- This is why do we extra processing in toJSON instance.
    , _listMutexFiltersConfig :: Maybe MutexFiltersConfig

    , _entityBeingEditedStateVar :: !String
    , _entitiesToShowRenderVar :: !String
    }

instance ToJSON EntityListTemplateData where
    toJSON td = object
        [ "listName" .= _listName td
        , "entityName" .= _entityName td
        , "entityClassName" .= _entityClassName td
        , "entityLowerName" .= _entityLowerName td
        , "showHeader" .= _listShowHeader td
        , "listFields" .= _listFields td
        , "mutexFiltersConfig" .= mfcConfigJSON
        , "entityBeingEditedStateVar" .= _entityBeingEditedStateVar td
        , "entitiesToShowRenderVar" .= _entitiesToShowRenderVar td
        ]
        where
            -- NOTE(matija): We have to explicitly make sure that the value here is false,
            -- because only then (and for the empty list) is the inverted section triggered.
            -- Although the input value might be Nothing, it still won't work because the value
            -- will be null.
            mfcConfigJSON = maybe (Aeson.Bool False) toJSON (_listMutexFiltersConfig td)

-- * List field template data

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

-- * List filter template data

data MutexFiltersConfig = MutexFiltersConfig
    { _mfcFilters :: ![ListFilterTemplateData]
    , _mfcNoFilterLabel :: !String
    }

instance ToJSON MutexFiltersConfig where
    toJSON mfc = object
        [ "filters" .= _mfcFilters mfc
        , "noFilterLabel" .= _mfcNoFilterLabel mfc
        ]

createMutexFiltersConfig :: [WEL.Filter] -> Maybe MutexFiltersConfig
createMutexFiltersConfig fs = case (length fs) of
    0 -> Nothing
    _ -> Just $ MutexFiltersConfig
        { _mfcFilters = map createListFilterTD fs 
        -- NOTE(matija): hardcoded for now, in the future this will
        -- be an option given by the user.
        , _mfcNoFilterLabel = "all"
        }

data ListFilterTemplateData = ListFilterTemplateData
    { _filterName :: !String
    , _filterPredicate :: !Wasp.JsCode.JsCode
    }

instance ToJSON ListFilterTemplateData where
    toJSON f = object
        [ "name" .= _filterName f
        , "predicate" .= _filterPredicate f
        ]

createListFilterTD :: WEL.Filter -> ListFilterTemplateData
createListFilterTD f = ListFilterTemplateData
    { _filterName = WEL._filterName f
    , _filterPredicate = WEL._filterPredicate f
    }

createEntityListTemplateData :: Wasp.Entity -> WEL.EntityList -> EntityListTemplateData
createEntityListTemplateData entity entityList =
    assert (Wasp.entityName entity == WEL._entityName entityList) $

        EntityListTemplateData
            { _listName = WEL._name entityList
            , _entityName = Wasp.entityName entity
            , _entityClassName = EC.getEntityClassName entity
            , _entityLowerName = EC.getEntityLowerName entity
            , _listShowHeader = showHeader
            , _listFields = map (createListFieldTD entity entityList) $ Wasp.entityFields entity
            , _listMutexFiltersConfig = createMutexFiltersConfig $ WEL._mutexFilters entityList
            , _entityBeingEditedStateVar = entityLowerName ++ "BeingEdited"
            , _entitiesToShowRenderVar = entityLowerName ++ "ListToShow"
            }
            where
                entityLowerName = EC.getEntityLowerName entity
                showHeader = maybe True id (WEL._showHeader entityList)

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

    templateSrcPath = EC.entityTemplatesDirPath </> SP.fromPathRelFile [P.relfile|components/List.js|]

    dstPath = Common.webAppSrcDirInProjectRootDir </> (entityListPathInSrc entity entityList)

    templateData = toJSON $ createEntityListTemplateData entity entityList

-- | Path in the generated src dir where the given entity list will be located.
entityListPathInSrc :: Wasp.Entity -> WEL.EntityList -> Path (Rel Common.WebAppSrcDir) File
entityListPathInSrc entity entityList =
    EC.entityComponentsDirPathInSrc entity </>
    (fromJust $ SP.parseRelFile $ (WEL._name entityList) ++ ".js")
