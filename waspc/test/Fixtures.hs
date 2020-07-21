module Fixtures where

import Wasp
import qualified Wasp.EntityForm as EF
import qualified Generator.WebAppGenerator.EntityGenerator.EntityFormGenerator as GEF

app :: App
app = App
    { appName = "test_app"
    , appTitle = "Hello World!"
    }

taskEntity :: Entity
taskEntity = Entity
    { entityName = "Task"
    , entityFields =
        [ Wasp.EntityField "description" Wasp.EftString
        , Wasp.EntityField "isDone" Wasp.EftBoolean
        ]
    }

taskCreateForm :: EF.EntityForm
taskCreateForm = EF.EntityForm
    { EF._name = "CreateTaskForm"
    , EF._entityName = "Task"
    , EF._submit = Just EF.Submit
        { EF._onEnter = Just False
        , EF._submitButton = Nothing
        }
    , EF._fields = []
    }

userEntity :: Entity
userEntity = Entity
    { entityName = "User"
    , entityFields =
        [ Wasp.EntityField "name" Wasp.EftString
        , Wasp.EntityField "surname" Wasp.EftString
        ]
    }

userCreateForm :: EF.EntityForm
userCreateForm = EF.EntityForm
    { EF._name = "CreateUserForm"
    , EF._entityName = "User"
    , EF._submit = Nothing
    , EF._fields = []
    }

wasp :: Wasp
wasp = fromWaspElems
    [ WaspElementApp app
    , WaspElementEntity taskEntity
    , WaspElementEntityForm taskCreateForm
    ]

formFieldIsDone :: GEF.FormFieldTemplateData
formFieldIsDone = GEF.FormFieldTemplateData
    { GEF._fieldName = "isDone"
    , GEF._fieldType = Wasp.EftBoolean
    , GEF._fieldShow = True
    , GEF._fieldDefaultValue = EF.DefaultValueBool True
    , GEF._fieldPlaceholder = Nothing
    , GEF._fieldLabel = Nothing
    }
