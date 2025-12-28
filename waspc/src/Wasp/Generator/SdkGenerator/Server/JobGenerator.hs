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
import StrongPath (Dir', File', Path, Path', Posix, Rel, Rel', fromRelFileP, parseRelFile, reldir, relfile, relfileP, (</>))
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.JSON as AS.JSON
import Wasp.AppSpec.Job (Job, JobExecutor (PgBoss))
import qualified Wasp.AppSpec.Job as J
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkProject (..),
    SdkTemplatesProjectDir,
    makeSdkImportPath,
    makeSdkProjectTmplFd,
    makeSdkProjectTmplFdWithData,
    makeSdkProjectTmplFdWithDestAndData,
  )
import Wasp.Generator.SdkGenerator.Server.Common (serverTemplatesDirInSdkTemplatesDir)
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
  return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = serverJobsDirInSdkTemplatesProjectDir </> [relfile|index.ts|]
    tmplData = object ["jobs" .= map getJobTmplData jobs]
    getJobTmplData (jobName, _) =
      object
        [ "typeName" .= toUpperFirst jobName,
          "jobName" .= jobName
        ]

genJob :: (String, Job) -> Generator FileDraft
genJob (jobName, job) =
  return $ makeSdkProjectTmplFdWithDestAndData destFile SdkUserCoreProject tmplFile (Just tmplData)
  where
    destFile = [reldir|server/jobs|] </> fromJust (parseRelFile $ jobName ++ ".ts")
    tmplFile = serverJobsDirInSdkTemplatesProjectDir </> [relfile|_job.ts|]
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
    sequence
      [ genServerJobFileCopy SdkUserCoreProject [relfile|core/job.ts|],
        genServerJobFileCopy SdkUserCoreProject [relfile|core/pgBoss/pgBoss.ts|],
        genServerJobFileCopy SdkUserCoreProject [relfile|core/pgBoss/pgBossJob.ts|],
        genServerJobFileCopy SdkUserCoreProject [relfile|core/pgBoss/types.ts|],
        genServerJobFileCopy SdkUserCoreProject [relfile|core/pgBoss/index.ts|]
      ]

-- NOTE: Our pg-boss related documentation references this version in URLs.
-- Please update the docs when this changes (until we solve: https://github.com/wasp-lang/wasp/issues/596).
pgBossVersionRange :: SV.Range
pgBossVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 8 4 2)]

pgBossDependency :: Npm.Dependency.Dependency
pgBossDependency = Npm.Dependency.make ("pg-boss", show pgBossVersionRange)

depsRequiredByJobs :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByJobs spec = [pgBossDependency | isPgBossJobExecutorUsed spec]

serverJobsDirInSdkTemplatesProjectDir :: Path' (Rel SdkTemplatesProjectDir) Dir'
serverJobsDirInSdkTemplatesProjectDir = serverTemplatesDirInSdkTemplatesDir </> [reldir|jobs|]

genServerJobFileCopy :: SdkProject -> Path' Rel' File' -> Generator FileDraft
genServerJobFileCopy sdkProject =
  return . makeSdkProjectTmplFd sdkProject . (serverJobsDirInSdkTemplatesProjectDir </>)
