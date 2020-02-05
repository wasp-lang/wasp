module Generator.Entity.EntityForm
    ( generateEntityCreateForm

      -- For testing
    , entityCreateFormPathInSrc
    , FormFieldTemplateData(..)
    ) where

import Control.Exception (assert)
import Data.Aeson ((.=), object, ToJSON(..))
import Data.Maybe (fromJust)
import Path ((</>), reldir, relfile)
import qualified Path
import qualified Path.Aliases as Path
import qualified Util

import qualified Wasp as Wasp
import Wasp (Wasp)

import qualified Wasp.EntityForm as WEF
import qualified Generator.FileDraft as FD
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
        [ "formName" .= _formName td
        , "entityClassName" .= _entityClassName td
        , "formFields" .=  _formFields td
        , "showSubmitButton" .= _showSubmitButton td
        ]

-- | Represents template data for the individual form field.
data FormFieldTemplateData = FormFieldTemplateData
    { _fieldName :: !String
    , _fieldType :: !Wasp.EntityFieldType
    , _fieldShow :: !Bool
    , _fieldDefaultValue :: !WEF.DefaultValue
    , _fieldPlaceholder :: Maybe String
    , _fieldLabel :: Maybe String
    } deriving (Show)

instance ToJSON FormFieldTemplateData where
    toJSON f = EC.addEntityFieldTypeToJsonAsKeyWithValueTrue (_fieldType f) $
        object
            [ "name" .= _fieldName f
            , "type" .= _fieldType f
            , "show" .= _fieldShow f
            , "defaultValue" .= case (_fieldDefaultValue f) of
                (WEF.DefaultValueString s) -> s
                (WEF.DefaultValueBool b) -> Util.toLowerFirst $ show b
            , "placeholder" .= _fieldPlaceholder f
            , "label" .= _fieldLabel f
            ]
    
-- | Given entity and an entity form for it, creates a single data structure
-- with all the values needed by the template to generate the form.
createEntityFormTemplateData :: Wasp.Entity -> WEF.EntityForm -> EntityFormTemplateData
createEntityFormTemplateData entity entityForm = 
    assert (Wasp.entityName entity == WEF._entityName entityForm) $

        EntityFormTemplateData
            { _formName = WEF._name entityForm
            , _entityClassName = EC.getEntityClassName entity
            , _formFields = map (createFormFieldTD entityForm) $ Wasp.entityFields entity
            -- Submit
            , _showSubmitButton = maybe True id maybeShowSubmitButton
            }
    where
        maybeShowSubmitButton :: Maybe Bool
        maybeShowSubmitButton = WEF._submit entityForm >>= WEF._submitButton >>= WEF._submitButtonShow

-- | Given field data from entity and an entity form for it, creates a single
-- data structure with all the values needed by the template to generate a form field.
createFormFieldTD :: WEF.EntityForm -> Wasp.EntityField -> FormFieldTemplateData
createFormFieldTD entityForm entityField = FormFieldTemplateData
    { _fieldName = Wasp.entityFieldName entityField
    , _fieldType = Wasp.entityFieldType entityField
    , _fieldShow = maybe True id $ formFieldConfig >>= WEF._fieldShow
    , _fieldDefaultValue = maybe 
                            defaultValueIfNothingInForm
                            id 
                            $ formFieldConfig >>= WEF._fieldDefaultValue
    , _fieldPlaceholder = formFieldConfig >>= WEF._fieldPlaceholder
    , _fieldLabel = fieldLabel
    }
    where
        -- Configuration of a form field within entity-form, if there is any.
        formFieldConfig :: Maybe WEF.Field
        formFieldConfig = WEF.getConfigForField entityForm entityField

        getDefaultValueForFieldWithType :: Wasp.EntityFieldType -> WEF.DefaultValue
        getDefaultValueForFieldWithType efType = case efType of
            Wasp.EftString -> WEF.DefaultValueString ""
            Wasp.EftBoolean -> WEF.DefaultValueBool False

        -- If user did not explicitly set a default value, we determine it ourselves.
        defaultValueIfNothingInForm :: WEF.DefaultValue
        defaultValueIfNothingInForm =
            getDefaultValueForFieldWithType $ Wasp.entityFieldType entityField

        fieldLabel :: Maybe String
        fieldLabel = case (formFieldConfig >>= WEF._fieldLabel) of
            -- Label property is not provided -> in that case we set label to the
            -- default value of entity field name (e.g. "description").
            Nothing             -> Just $ Wasp.entityFieldName entityField
            -- Label property is provided and explicitly disabled ('label: none').
            Just Nothing        -> Nothing
            -- Label property is provided and set to a specific value ('label: "something"').
            Just (Just label)   -> Just label


-- | Generates entity creation form.
generateEntityCreateForm :: Wasp -> WEF.EntityForm -> FD.FileDraft
generateEntityCreateForm wasp entityForm =
    FD.createTemplateFileDraft dstPath templateSrcPath (Just templateData)
  where
    -- NOTE(matija): There should always be an entity in wasp for the given entity form.
    -- If not, we want an error to be thrown.
    entity = maybe
        (error $ "Wasp must contain entity to which the entity form refers: " ++
            WEF._entityName entityForm)
        id
        (Wasp.getEntityByName wasp (WEF._entityName entityForm))

    templateSrcPath = EC.entityTemplatesDirPath </> [reldir|components|] </> [relfile|CreateForm.js|]

    dstPath = Common.srcDirPath </> (entityCreateFormPathInSrc entity entityForm)

    templateData = toJSON $ createEntityFormTemplateData entity entityForm

entityCreateFormPathInSrc :: Wasp.Entity -> WEF.EntityForm -> Path.RelFile
entityCreateFormPathInSrc entity entityForm =
    EC.entityComponentsDirPathInSrc entity </>
    (fromJust $ Path.parseRelFile $ (WEF._name entityForm) ++ ".js")
