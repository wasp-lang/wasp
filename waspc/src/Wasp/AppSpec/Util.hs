module Wasp.AppSpec.Util
  ( isPgBossJobExecutorUsed,
    findRoutePathFromRef,
  )
where

import Data.List (find)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Core.Ref as AS.Ref
import qualified Wasp.AppSpec.Job as Job
import qualified Wasp.AppSpec.Route as AS.Route

isPgBossJobExecutorUsed :: AppSpec -> Bool
isPgBossJobExecutorUsed spec = any (\(_, job) -> Job.executor job == Job.PgBoss) (AS.getJobs spec)

findRoutePathFromRef :: AS.AppSpec -> AS.Ref.Ref AS.Route.Route -> Maybe String
findRoutePathFromRef spec (AS.Ref.Ref routeName) = maybePath
  where
    maybeRoute = find ((==) routeName . fst) (AS.getRoutes spec)
    maybePath = AS.Route.path . snd <$> maybeRoute