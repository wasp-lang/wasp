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
    Rel,
    parseRelFile,
    reldir,
    reldirP,
    relfile,
    toFilePath,
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.JSON as AS.JSON
import Wasp.AppSpec.Job (Job, JobExecutor (PgBoss, Simple), jobExecutors)
import qualified Wasp.AppSpec.Job as J
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getServerJsImport)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common
  ( ServerRootDir,
    ServerTemplatesDir,
    srcDirInServerTemplatesDir,
  )
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.SemanticVersion as SV

genJobs :: AppSpec -> Generator [FileDraft]
genJobs spec = return $ genAllJobImports spec : (genJob <$> getJobs spec)

genJob :: (String, Job) -> FileDraft
genJob (jobName, job) =
  C.mkTmplFdWithDstAndData
    tmplFile
    dstFile
    ( Just $
        object
          [ "jobName" .= jobName,
            "jobPerformFnName" .= jobPerformFnName,
            "jobPerformFnImportStatement" .= jobPerformFnImportStatement,
            -- NOTE: You cannot directly input an Aeson.object for Mustache to substitute.
            -- This is why we must get a text representation of the object, either by
            -- `Aeson.Text.encodeToLazyText` on an Aeson.Object, or `show` on an AS.JSON.
            "jobSchedule" .= Aeson.Text.encodeToLazyText (fromMaybe Aeson.Null maybeJobSchedule),
            "jobPerformOptions" .= show (fromMaybe AS.JSON.emptyObject maybeJobPerformOptions),
            "executorJobRelFP" .= toFilePath (executorJobTemplateInJobsDir (J.executor job)),
            "entities" .= maybe [] (map (C.buildEntityData . AS.refName)) (J.entities job)
          ]
    )
  where
    tmplFile = C.asTmplFile $ jobsDirInServerTemplatesDir SP.</> [relfile|_job.js|]
    dstFile = jobsDirInServerRootDir SP.</> fromJust (parseRelFile $ jobName ++ ".js")
    (jobPerformFnName, jobPerformFnImportStatement) = getServerJsImport [reldirP|../|] $ (J.fn . J.perform) job
    maybeJobPerformOptions = J.performExecutorOptionsJson job
    jobScheduleTmplData s =
      object
        [ "cron" .= J.cron s,
          "args" .= J.args s,
          "options" .= fromMaybe AS.JSON.emptyObject (J.scheduleExecutorOptionsJson job)
        ]
    maybeJobSchedule = jobScheduleTmplData <$> J.schedule job

-- Creates a file that is imported on the server to ensure all job JS modules are loaded
-- even if they are not referenced by user code. This ensures schedules are started, etc.
genAllJobImports :: AppSpec -> FileDraft
genAllJobImports spec =
  let tmplFile = C.asTmplFile $ jobsDirInServerTemplatesDir SP.</> [relfile|core/_allJobs.js|]
      dstFile = jobsDirInServerRootDir SP.</> [relfile|core/allJobs.js|]
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

genJobExecutors :: Generator [FileDraft]
genJobExecutors = return $ jobExecutorFds ++ jobExecutorHelperFds
  where
    jobExecutorFds :: [FileDraft]
    jobExecutorFds = genJobExecutor <$> jobExecutors

    genJobExecutor :: JobExecutor -> FileDraft
    genJobExecutor jobExecutor = C.mkTmplFd $ executorJobTemplateInServerTemplatesDir jobExecutor

    jobExecutorHelperFds :: [FileDraft]
    jobExecutorHelperFds =
      [ C.mkTmplFd $ jobsDirInServerTemplatesDir SP.</> [relfile|core/pgBoss/pgBoss.js|],
        C.mkTmplFd $ jobsDirInServerTemplatesDir SP.</> [relfile|core/Job.js|],
        C.mkTmplFd $ jobsDirInServerTemplatesDir SP.</> [relfile|core/SubmittedJob.js|]
      ]

data JobsDir

jobsDirInServerTemplatesDir :: Path' (Rel ServerTemplatesDir) (Dir JobsDir)
jobsDirInServerTemplatesDir = srcDirInServerTemplatesDir SP.</> [reldir|jobs|]

executorJobTemplateInServerTemplatesDir :: JobExecutor -> Path SP.System (Rel ServerTemplatesDir) File'
executorJobTemplateInServerTemplatesDir = (jobsDirInServerTemplatesDir SP.</>) . executorJobTemplateInJobsDir

executorJobTemplateInJobsDir :: JobExecutor -> Path' (Rel JobsDir) File'
executorJobTemplateInJobsDir PgBoss = [relfile|core/pgBoss/pgBossJob.js|]
executorJobTemplateInJobsDir Simple = [relfile|core/simpleJob.js|]

-- Path to destination files are the same as in templates dir.
jobsDirInServerRootDir :: Path' (Rel ServerRootDir) (Dir JobsDir)
jobsDirInServerRootDir = SP.castRel jobsDirInServerTemplatesDir

-- NOTE: Our pg-boss related documentation references this version in URLs.
-- Please update the docs when this changes (until we solve: https://github.com/wasp-lang/wasp/issues/596).
pgBossVersionRange :: SV.Range
pgBossVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 8 0 0)]

pgBossDependency :: AS.Dependency.Dependency
pgBossDependency = AS.Dependency.make ("pg-boss", show pgBossVersionRange)

depsRequiredByJobs :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByJobs spec = [pgBossDependency | isPgBossJobExecutorUsed spec]
