module Wasp.AppSpec.Util
  ( isPgBossJobExecutorUsed,
    getRoutePathFromRef,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Core.Ref as AS.Ref
import qualified Wasp.AppSpec.Job as Job
import qualified Wasp.AppSpec.Route as AS.Route

isPgBossJobExecutorUsed :: AppSpec -> Bool
isPgBossJobExecutorUsed spec = any (\(_, job) -> Job.executor job == Job.PgBoss) (AS.getJobs spec)

getRoutePathFromRef :: AS.AppSpec -> AS.Ref.Ref AS.Route.Route -> String
getRoutePathFromRef spec ref = path
  where
    route = AS.resolveRef spec ref
    path = AS.Route.path . snd $ route
