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
import StrongPath (Dir', File', Path, Path', Posix, Rel, Rel', castRel, fromRelFileP, parseRelFile, reldir, relfile, relfileP, (</>))
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
import Wasp.Generator.SdkGenerator.Common
  ( makeSdkImportPath,
  )
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( TemplatesSdkUserCoreProjectDir,
    mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDestAndData,
  )
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
genIndexTs jobs =
  return $ mkTmplFdWithData (serverJobsDirInTemplatesSdkUserCoreProjectDir </> [relfile|index.ts|]) tmplData
  where
    tmplData = object ["jobs" .= map getJobTmplData jobs]
    getJobTmplData (jobName, _) =
      object
        [ "typeName" .= toUpperFirst jobName,
          "jobName" .= jobName
        ]

genJob :: (String, Job) -> Generator FileDraft
genJob (jobName, job) =
  return $
    mkTmplFdWithDestAndData
      (castRel (serverJobsDirInTemplatesSdkUserCoreProjectDir </> fromJust (parseRelFile (jobName ++ ".ts"))))
      (serverJobsDirInTemplatesSdkUserCoreProjectDir </> [relfile|_job.ts|])
      (Just tmplData)
  where
    tmplData =
      object
        [ "jobName" .= jobName,
          "typeName" .= toUpperFirst jobName,
          "jobExecutorImportPath" .= fromRelFileP jobExecutorImportPath,
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
  _anyJob ->
    sequence $
      genFileCopyInServerJob [relfile|core/job.ts|]
        : concatMap genJobExecutor jobExecutors
    where
      -- Per each defined job executor, we generate the needed files.
      genJobExecutor :: JobExecutor -> [Generator FileDraft]
      genJobExecutor PgBoss =
        [ genFileCopyInServerJob [relfile|core/pgBoss/pgBoss.ts|],
          genFileCopyInServerJob [relfile|core/pgBoss/pgBossJob.ts|],
          genFileCopyInServerJob [relfile|core/pgBoss/types.ts|],
          genFileCopyInServerJob [relfile|core/pgBoss/index.ts|]
        ]

-- NOTE: Our pg-boss related documentation references this version in URLs.
-- Please update the docs when this changes (until we solve: https://github.com/wasp-lang/wasp/issues/596).
pgBossVersionRange :: SV.Range
pgBossVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 8 4 2)]

pgBossDependency :: Npm.Dependency.Dependency
pgBossDependency = Npm.Dependency.make ("pg-boss", show pgBossVersionRange)

depsRequiredByJobs :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByJobs spec = [pgBossDependency | isPgBossJobExecutorUsed spec]

serverJobsDirInTemplatesSdkUserCoreProjectDir :: Path' (Rel TemplatesSdkUserCoreProjectDir) Dir'
serverJobsDirInTemplatesSdkUserCoreProjectDir = [reldir|server/jobs|]

genFileCopyInServerJob :: Path' Rel' File' -> Generator FileDraft
genFileCopyInServerJob =
  return . mkTmplFd . (serverJobsDirInTemplatesSdkUserCoreProjectDir </>)
