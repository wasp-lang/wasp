module Wasp.Generator.SdkGenerator.Server.JobGenerator
  ( genNewJobsApi,
    genJobExecutors,
    depsRequiredByJobs,
    getJobExecutorImportPath,
    getImportJsonForJobDefinition,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import Data.Maybe (fromJust, fromMaybe)
import StrongPath (File', Path, Posix, Rel, reldir, relfile, relfileP, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.JSON as AS.JSON
import Wasp.AppSpec.Job (Job, JobExecutor (PgBoss), jobExecutors)
import qualified Wasp.AppSpec.Job as J
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (makeSdkImportPath)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.JsImport as JI
import qualified Wasp.SemanticVersion as SV
import Wasp.Util

genNewJobsApi :: AppSpec -> Generator [FileDraft]
genNewJobsApi spec =
  case getJobs spec of
    [] -> return []
    jobs ->
      sequence
        [ genIndexTs jobs
        ]
        <++> mapM genJob jobs
        <++> genJobExecutors spec

genIndexTs :: [(String, Job)] -> Generator FileDraft
genIndexTs jobs = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|server/jobs/index.ts|]
    tmplData = object ["jobs" .= map getJobTmplData jobs]
    getJobTmplData (jobName, _) =
      object
        [ "typeName" .= toUpperFirst jobName,
          "jobName" .= jobName
        ]

genJob :: (String, Job) -> Generator FileDraft
genJob (jobName, job) =
  return
    $ C.mkTmplFdWithDstAndData
      tmplFile
      dstFile
    $ Just tmplData
  where
    tmplFile = [relfile|server/jobs/_job.ts|]
    dstFile = [reldir|server/jobs|] </> fromJust (SP.parseRelFile $ jobName ++ ".ts")
    tmplData =
      object
        [ "jobName" .= jobName,
          "typeName" .= toUpperFirst jobName,
          "jobExecutorImportPath" .= SP.fromRelFileP jobExecutorImportPath,
          "entities" .= maybe [] (map (makeJsonWithEntityData . AS.refName)) (J.entities job),
          -- NOTE: You cannot directly input an Aeson.object for Mustache to substitute.
          -- This is why we must get a text representation of the object, either by
          -- `Aeson.Text.encodeToLazyText` on an Aeson.Object, or `show` on an AS.JSON.
          "jobSchedule" .= getJobScheduleData (J.schedule job),
          "jobPerformOptions" .= show (fromMaybe AS.JSON.emptyObject maybeJobPerformOptions)
        ]

    jobExecutorImportPath = getJobExecutorImportPath (J.executor job)

    maybeJobPerformOptions = J.performExecutorOptionsJson job

    getJobScheduleData =
      maybe
        (object ["isDefined" .= False])
        ( \schedule ->
            object
              [ "isDefined" .= True,
                "cron" .= J.cron schedule,
                "args" .= getJobScheduleArgs (J.args schedule),
                "options" .= getJobSchduleOptions (J.scheduleExecutorOptionsJson job)
              ]
        )
    getJobScheduleArgs =
      maybe
        (object ["isDefined" .= False])
        (\args -> object ["isDefined" .= True, "json" .= Aeson.Text.encodeToLazyText args])

    getJobSchduleOptions =
      maybe
        (object ["isDefined" .= False])
        (\options -> object ["isDefined" .= True, "json" .= Aeson.Text.encodeToLazyText options])

-- | We are importing relevant functions and types per executor e.g. JobFn or registerJob,
-- this functions maps the executor to the import path from SDK.
getJobExecutorImportPath :: JobExecutor -> Path Posix (Rel r) File'
getJobExecutorImportPath PgBoss = makeSdkImportPath [relfileP|server/jobs/core/pgBoss|]

getImportJsonForJobDefinition :: String -> Aeson.Value
getImportJsonForJobDefinition jobName =
  GJI.jsImportToImportJson $
    Just $
      JI.JsImport
        { JI._path = JI.ModuleImportPath $ makeSdkImportPath [relfileP|server/jobs|],
          JI._name = JI.JsImportField jobName,
          -- NOTE: We are using alias to avoid name conflicts with user defined imports.
          JI._importAlias = Just "_waspJobDefinition"
        }

genJobExecutors :: AppSpec -> Generator [FileDraft]
genJobExecutors spec = case getJobs spec of
  [] -> return []
  _someJobs ->
    return $
      C.mkTmplFd [relfile|server/jobs/core/job.ts|] : genAllJobExecutors
  where
    genAllJobExecutors = concatMap genJobExecutor jobExecutors

    -- Per each defined job executor, we generate the needed files.
    genJobExecutor :: JobExecutor -> [FileDraft]
    genJobExecutor PgBoss =
      [ C.mkTmplFd [relfile|server/jobs/core/pgBoss/pgBoss.ts|],
        C.mkTmplFd [relfile|server/jobs/core/pgBoss/pgBossJob.ts|],
        C.mkTmplFd [relfile|server/jobs/core/pgBoss/types.ts|],
        C.mkTmplFd [relfile|server/jobs/core/pgBoss/index.ts|]
      ]

-- NOTE: Our pg-boss related documentation references this version in URLs.
-- Please update the docs when this changes (until we solve: https://github.com/wasp-lang/wasp/issues/596).
pgBossVersionRange :: SV.Range
pgBossVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 8 4 2)]

pgBossDependency :: Npm.Dependency.Dependency
pgBossDependency = Npm.Dependency.make ("pg-boss", show pgBossVersionRange)

depsRequiredByJobs :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByJobs spec = [pgBossDependency | isPgBossJobExecutorUsed spec]
