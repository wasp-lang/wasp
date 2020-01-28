module Generator.Entity.EntityForm
    ( generateEntityCreateForm

      -- For testing
    , entityCreateFormPathInSrc
    , FormFieldTemplateData(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))
import Control.Exception (assert)
import Data.Maybe (fromJust)
import Path ((</>), reldir, relfile)
import qualified Path
import qualified Path.Aliases as Path
import qualified Util

import qualified Wasp as Wasp
import Wasp (Wasp)

import qualified Wasp.EntityForm as EF
import Generator.FileDraft
import qualified Generator.Entity.Common as EC
import qualified Generator.Common as Common

-- | Data which will be fed to the Mustache "create form" template.
data EntityFormTemplateData = EntityFormTemplateData
    { _formName :: !String
    , _entityClassName :: !String
    , _formFields :: ![FormFieldTemplateData]
    -- Submit
    , _showSubmitButton :: !Bool
    } deriving (Show)

instance ToJSON EntityFormTemplateData where
    toJSON td = object
        [ "name" .= _formName td
        , "entityClassName" .= _entityClassName td
        , "formFields" .=  _formFields td
        , "showSubmitButton" .= _showSubmitButton td
        ]

-- | Represents template data for the individual form field.
data FormFieldTemplateData = FormFieldTemplateData
    { _fieldName :: !String
    , _fieldType :: Wasp.EntityFieldType
    , _fieldShow :: !Bool
    , _fieldDefaultValue :: !EF.DefaultValue
    } deriving (Show)

instance ToJSON FormFieldTemplateData where
    toJSON f = EC.addEntityFieldTypeToJsonAsKeyWithValueTrue (_fieldType f) $
        object
            [ "name" .= _fieldName f
            , "type" .= _fieldType f
            , "show" .= _fieldShow f
            , "defaultValue" .= case (_fieldDefaultValue f) of
                (EF.DefaultValueString s) -> s
                (EF.DefaultValueBool b) -> Util.toLowerFirst $ show b
            ]
    
-- | Given entity and an entity form for it, creates a single data structure
-- with all the values needed by the template to generate the form.
createEntityFormTemplateData :: Wasp.Entity -> EF.EntityForm -> EntityFormTemplateData
createEntityFormTemplateData entity entityForm = 
    assert (Wasp.entityName entity == EF._entityName entityForm) $

        EntityFormTemplateData
            { _formName = EF._name entityForm
            , _entityClassName = EC.getEntityClassName entity
            , _formFields = map (createFormField entityForm) $ Wasp.entityFields entity
            -- Submit
            , _showSubmitButton = maybe True id maybeShowSubmitButton
            }
    where
        maybeShowSubmitButton :: Maybe Bool
        maybeShowSubmitButton = EF._submit entityForm >>= EF._submitButton >>= EF._submitButtonShow

-- | Given field data from entity and an entity form for it, creates a single
-- data structure with all the values needed by the template to generate a form field.
createFormField :: EF.EntityForm -> Wasp.EntityField -> FormFieldTemplateData
createFormField entityForm entityField = FormFieldTemplateData
    { _fieldName = Wasp.entityFieldName entityField
    , _fieldType = Wasp.entityFieldType entityField
    , _fieldShow = maybe True id $ formFieldConfig >>= EF._fieldShow
    , _fieldDefaultValue = maybe 
                            defaultValueIfNothingInForm
                            id 
                            $ formFieldConfig >>= EF._fieldDefaultValue
    }
    where
        -- Configuration of a form field within entity-form, if there is any.
        formFieldConfig :: Maybe EF.Field
        formFieldConfig = EF.getConfigForField entityForm entityField

        getDefaultValueForFieldWithType :: Wasp.EntityFieldType -> EF.DefaultValue
        getDefaultValueForFieldWithType efType = case efType of
            Wasp.EftString -> EF.DefaultValueString ""
            Wasp.EftBoolean -> EF.DefaultValueBool False

        -- If user did not explicitly set a default value, we determine it ourselves.
        defaultValueIfNothingInForm :: EF.DefaultValue
        defaultValueIfNothingInForm =
            getDefaultValueForFieldWithType $ Wasp.entityFieldType entityField


-- | Generates entity creation form.
generateEntityCreateForm :: Wasp -> EF.EntityForm -> FileDraft
generateEntityCreateForm wasp entityForm =
    createTemplateFileDraft dstPath templateSrcPath (Just templateData)
  where
    -- NOTE(matija): There should always be an entity in wasp for the given entity form,
    -- we want an error to be thrown otherwise.
    entity = maybe
        (error $ "Wasp must contain entity to which the entity form refers: " ++
            EF._entityName entityForm)
        id
        (Wasp.getEntityByName wasp (EF._entityName entityForm))

    templateSrcPath = EC.entityTemplatesDirPath </> [reldir|components|] </> [relfile|CreateForm.js|]

    dstPath = Common.srcDirPath </> (entityCreateFormPathInSrc entity entityForm)

    templateData = toJSON $ createEntityFormTemplateData entity entityForm

entityCreateFormPathInSrc :: Wasp.Entity -> EF.EntityForm -> Path.RelFile
entityCreateFormPathInSrc entity entityForm =
    EC.entityComponentsDirPathInSrc entity </>
    (fromJust $ Path.parseRelFile $ (EF._name entityForm) ++ ".js")
