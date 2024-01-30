module Wasp.Generator.ServerGenerator.JobGenerator
  ( genJobs,
    genJobExecutors,
    pgBossVersionRange,
    pgBossDependency,
    depsRequiredByJobs,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import Data.Maybe (fromJust, fromMaybe)
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    reldir,
    reldirP,
    relfile,
    toFilePath,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.JSON as AS.JSON
import Wasp.AppSpec.Job (Job, JobExecutor (PgBoss), jobExecutors)
import qualified Wasp.AppSpec.Job as J
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.Generator.Common (ServerRootDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Server.JobGenerator (getImportPathForJobName, getJobExecutorTypesImportPath)
import Wasp.Generator.ServerGenerator.Common
  ( ServerTemplatesDir,
    srcDirInServerTemplatesDir,
  )
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Generator.ServerGenerator.JsImport as SJI
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (ModuleImportPath), makeJsImport)
import qualified Wasp.JsImport as JI
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (toUpperFirst)

genJobs :: AppSpec -> Generator [FileDraft]
genJobs spec = case getJobs spec of
  [] -> return []
  jobs -> return $ genAllJobImports spec : (genJob <$> jobs)

genJob :: (String, Job) -> FileDraft
genJob (jobName, job) =
  C.mkTmplFdWithDstAndData
    tmplFile
    dstFile
    ( Just $
        object
          [ "jobName" .= jobName,
            "typeName" .= toUpperFirst jobName,
            "jobPerformFnName" .= jobPerformFnName,
            "jobPerformFnImportStatement" .= jobPerformFnImportStatement,
            -- NOTE: You cannot directly input an Aeson.object for Mustache to substitute.
            -- This is why we must get a text representation of the object, either by
            -- `Aeson.Text.encodeToLazyText` on an Aeson.Object, or `show` on an AS.JSON.
            "jobSchedule" .= Aeson.Text.encodeToLazyText (fromMaybe Aeson.Null maybeJobSchedule),
            "jobPerformOptions" .= show (fromMaybe AS.JSON.emptyObject maybeJobPerformOptions),
            "jobExecutorRelativePath" .= toFilePath (executorJobTemplateInJobsDir "js" (J.executor job)),
            "jobExecutorTypesImportPath" .= SP.fromRelFileP jobExecutorTypesImportPath,
            "jobEntitiesImportStatement" .= jobEntitiesImportStatement,
            "jobEntitiesIdentifier" .= jobEntitiesIdentifier
          ]
    )
  where
    tmplFile = C.asTmplFile $ jobsDirInServerTemplatesDir </> [relfile|_job.ts|]
    dstFile = jobsDirInServerRootDir </> fromJust (SP.parseRelFile $ jobName ++ ".ts")

    -- Users import job types from the SDK, so the types for each job are generated
    -- separately and imported from the SDK.
    (jobEntitiesImportStatement, jobEntitiesIdentifier) =
      JI.getJsImportStmtAndIdentifier $
        makeJsImport (ModuleImportPath $ getImportPathForJobName jobName) (JsImportField "entities")

    (jobPerformFnImportStatement, jobPerformFnName) =
      SJI.getJsImportStmtAndIdentifier relPathFromJobsDirToServerSrcDir $ (J.fn . J.perform) job

    maybeJobPerformOptions = J.performExecutorOptionsJson job
    jobScheduleTmplData s =
      object
        [ "cron" .= J.cron s,
          "args" .= J.args s,
          "options" .= fromMaybe AS.JSON.emptyObject (J.scheduleExecutorOptionsJson job)
        ]
    maybeJobSchedule = jobScheduleTmplData <$> J.schedule job

    jobExecutorTypesImportPath = getJobExecutorTypesImportPath (J.executor job)

    relPathFromJobsDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromJobsDirToServerSrcDir = [reldirP|../|]

-- Creates a file that is imported on the server to ensure all job JS modules are loaded
-- even if they are not referenced by user code. This ensures schedules are started, etc.
genAllJobImports :: AppSpec -> FileDraft
genAllJobImports spec =
  let tmplFile = C.asTmplFile $ jobsDirInServerTemplatesDir </> [relfile|core/_allJobs.ts|]
      dstFile = jobsDirInServerRootDir </> [relfile|core/allJobs.ts|]
   in C.mkTmplFdWithDstAndData
        tmplFile
        dstFile
        ( Just $
            object
              ["jobs" .= (buildJobInfo <$> (fst <$> getJobs spec))]
        )
  where
    buildJobInfo :: String -> Aeson.Value
    buildJobInfo jobName =
      object
        [ "name" .= jobName
        ]

genJobExecutors :: AppSpec -> Generator [FileDraft]
genJobExecutors spec = case getJobs spec of
  [] -> return []
  _someJobs -> return $ jobExecutorFds ++ jobExecutorHelperFds
  where
    jobExecutorFds :: [FileDraft]
    jobExecutorFds = genJobExecutor <$> jobExecutors

    genJobExecutor :: JobExecutor -> FileDraft
    genJobExecutor jobExecutor = C.mkTmplFd $ executorJobTemplateInServerTemplatesDir jobExecutor

    jobExecutorHelperFds :: [FileDraft]
    jobExecutorHelperFds =
      [ C.mkTmplFd $ jobsDirInServerTemplatesDir </> [relfile|core/pgBoss/pgBoss.ts|],
        C.mkTmplFd $ jobsDirInServerTemplatesDir </> [relfile|core/job.ts|]
      ]

    executorJobTemplateInServerTemplatesDir :: JobExecutor -> Path SP.System (Rel ServerTemplatesDir) File'
    executorJobTemplateInServerTemplatesDir = (jobsDirInServerTemplatesDir </>) . executorJobTemplateInJobsDir "ts"

data JobsDir

jobsDirInServerTemplatesDir :: Path' (Rel ServerTemplatesDir) (Dir JobsDir)
jobsDirInServerTemplatesDir = srcDirInServerTemplatesDir </> [reldir|jobs|]

executorJobTemplateInJobsDir :: String -> JobExecutor -> Path' (Rel JobsDir) File'
executorJobTemplateInJobsDir ext PgBoss = fromJust $ SP.parseRelFile $ "core/pgBoss/pgBossJob" <> "." <> ext

-- Path to destination files are the same as in templates dir.
jobsDirInServerRootDir :: Path' (Rel ServerRootDir) (Dir JobsDir)
jobsDirInServerRootDir = SP.castRel jobsDirInServerTemplatesDir

-- NOTE: Our pg-boss related documentation references this version in URLs.
-- Please update the docs when this changes (until we solve: https://github.com/wasp-lang/wasp/issues/596).
pgBossVersionRange :: SV.Range
pgBossVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 8 4 2)]

pgBossDependency :: AS.Dependency.Dependency
pgBossDependency = AS.Dependency.make ("pg-boss", show pgBossVersionRange)

depsRequiredByJobs :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByJobs spec = [pgBossDependency | isPgBossJobExecutorUsed spec]
