module Wasp.AppSpec.Util
  ( isPgBossJobExecutorUsed,
    getRoutePathFromRef,
    hasEntities,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Core.Ref as AS.Ref
import qualified Wasp.AppSpec.Job as Job
import qualified Wasp.AppSpec.Route as AS.Route

isPgBossJobExecutorUsed :: AppSpec -> Bool
isPgBossJobExecutorUsed spec = any ((== Job.PgBoss) . Job.executor) (AS.getJobs spec)

getRoutePathFromRef :: AS.AppSpec -> AS.Ref.Ref AS.Route.Route -> String
getRoutePathFromRef spec ref = route.path
  where
    route = AS.resolveRef spec ref

hasEntities :: AppSpec -> Bool
hasEntities spec = not . null $ AS.getEntities spec
