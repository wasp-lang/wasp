module Wasp.Generator.ServerGenerator.JobGenerator
  ( genJobs,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath
  ( Dir,
    Path,
    Path',
    Posix,
    Rel,
    reldir,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getJobs)
import Wasp.AppSpec.Job (Job)
import qualified Wasp.AppSpec.Job as J
import Wasp.Generator.Common (ServerRootDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Server.JobGenerator
  ( getImportJsonForJobDefinition,
    getJobExecutorImportPath,
  )
import Wasp.Generator.ServerGenerator.Common
  ( ServerTemplatesDir,
    srcDirInServerTemplatesDir,
  )
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Generator.ServerGenerator.JsImport as SJI

genJobs :: AppSpec -> Generator [FileDraft]
genJobs spec = case getJobs spec of
  [] -> return []
  jobs -> return $ genAllJobImports spec : (genRegisterJob <$> jobs)

genRegisterJob :: (String, Job) -> FileDraft
genRegisterJob (jobName, job) =
  C.mkTmplFdWithDstAndData
    tmplFile
    dstFile
    ( Just $
        object
          [ "jobPerformFn" .= jobPerformFn,
            "jobExecutorImportPath" .= SP.fromRelFileP (getJobExecutorImportPath (J.executor job)),
            "jobDefinition" .= getImportJsonForJobDefinition jobName
          ]
    )
  where
    tmplFile = C.asTmplFile $ jobsDirInServerTemplatesDir </> [relfile|_job.ts|]
    dstFile = jobsDirInServerRootDir </> fromJust (SP.parseRelFile $ jobName ++ ".ts")

    jobPerformFn =
      SJI.extImportToImportJson relPathFromJobsDirToServerSrcDir $
        Just $
          (J.fn . J.perform) job

    relPathFromJobsDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromJobsDirToServerSrcDir = [reldirP|../|]

-- Creates a file that is imported on the server to ensure all job JS modules are loaded
-- even if they are not referenced by user code. This ensures schedules are started, etc.
genAllJobImports :: AppSpec -> FileDraft
genAllJobImports spec =
  let tmplFile = C.asTmplFile $ jobsDirInServerTemplatesDir </> [relfile|core/allJobs.ts|]
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
    buildJobInfo jobName = object ["name" .= jobName]

data JobsDir

jobsDirInServerTemplatesDir :: Path' (Rel ServerTemplatesDir) (Dir JobsDir)
jobsDirInServerTemplatesDir = srcDirInServerTemplatesDir </> [reldir|jobs|]

-- Path to destination files are the same as in templates dir.
jobsDirInServerRootDir :: Path' (Rel ServerRootDir) (Dir JobsDir)
jobsDirInServerRootDir = SP.castRel jobsDirInServerTemplatesDir
