module Wasp.Generator.SdkGenerator.Server.TasksG
  ( genTasks,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (nub)
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec, getTasks)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Task as AS.Task
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (mkTmplFdWithData)
import Wasp.Util (toUpperFirst)

genTasks :: AppSpec -> Generator [FileDraft]
genTasks spec = case getTasks spec of
  [] -> return []
  tasks -> return [mkTmplFdWithData [relfile|server/tasks/index.ts|] (tmplData tasks)]
  where
    tmplData tasks =
      object
        [ "tasks" .= map getTaskTmplData tasks,
          "allEntities" .= nub (concatMap getTaskEntities tasks)
        ]

    getTaskTmplData :: AS.Task.Task -> Aeson.Value
    getTaskTmplData task =
      object
        [ "typeName" .= toUpperFirst task.name,
          "entities" .= getTaskEntities task
        ]

    getTaskEntities :: AS.Task.Task -> [Aeson.Value]
    getTaskEntities task = maybe [] (map (makeJsonWithEntityData . AS.refName)) task.entities
