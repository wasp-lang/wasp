module Fixtures where

import Wasp

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

wasp :: Wasp
wasp = fromWaspElems
    [ WaspElementEntity taskEntity
    , WaspElementEntityForm taskCreateForm
    ]
