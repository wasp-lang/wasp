module Fixtures where

import Wasp

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

taskCreateForm :: EntityForm
taskCreateForm = EntityForm
    { efName = "CreateTaskForm"
    , efEntityName = "Task"
    , efSubmitConfig = Just EntityFormSubmitConfig
        { onEnter = False
        }
    }

userEntity :: Entity
userEntity = Entity
    { entityName = "User"
    , entityFields =
        [ Wasp.EntityField "name" Wasp.EftString
        , Wasp.EntityField "surname" Wasp.EftString
        ]
    }

userCreateForm :: EntityForm
userCreateForm = EntityForm
    { efName = "CreateUserForm"
    , efEntityName = "User"
    , efSubmitConfig = Nothing
    }

wasp :: Wasp
wasp = fromWaspElems
    [ WaspElementApp app
    , WaspElementEntity taskEntity
    , WaspElementEntityForm taskCreateForm
    ]
