module Wasp.AppSpec.Util
  ( isPgBossJobExecutorUsed,
    hasEntities,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Job as Job

isPgBossJobExecutorUsed :: AppSpec -> Bool
isPgBossJobExecutorUsed spec = any (\(_, job) -> Job.executor job == Job.PgBoss) (AS.getJobs spec)

hasEntities :: AppSpec -> Bool
hasEntities spec = not . null $ AS.getEntities spec
