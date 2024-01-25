module Wasp.Generator.SdkGenerator.JobGenerator (genJobTypes, getImportPathForJobName, getJobExecutorTypesImportPath) where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (File', Path, Posix, Rel, reldir, relfile, relfileP, (</>))
import qualified StrongPath as SP
import StrongPath.TH (reldirP)
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Job (Job, JobExecutor (PgBoss))
import qualified Wasp.AppSpec.Job as J
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (makeSdkImportPath)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Util

genJobTypes :: AppSpec -> Generator [FileDraft]
genJobTypes spec = case getJobs spec of
  [] -> return []
  jobs -> return $ map genJobType jobs

genJobType :: (String, Job) -> FileDraft
genJobType (jobName, job) =
  C.mkTmplFdWithDstAndData
    tmplFile
    dstFile
    $ Just tmplData
  where
    tmplFile = [relfile|jobs/_jobTypes.ts|]
    dstFile = [reldir|jobs|] </> fromJust (SP.parseRelFile $ jobName ++ ".ts")
    tmplData =
      object
        [ "typeName" .= toUpperFirst jobName,
          "jobExecutorTypesImportPath" .= SP.fromRelFileP jobExecutorTypesImportPath,
          "entities" .= maybe [] (map (makeJsonWithEntityData . AS.refName)) (J.entities job)
        ]

    jobExecutorTypesImportPath = getJobExecutorTypesImportPath (J.executor job)

getImportPathForJobName :: String -> Path Posix (Rel d) File'
getImportPathForJobName jobName = makeSdkImportPath $ [reldirP|jobs|] </> fromJust (SP.parseRelFileP jobName)

-- | We are importing relevant types per executor e.g. JobFn, this functions maps
--   the executor to the import path of the relevant types.
getJobExecutorTypesImportPath :: JobExecutor -> Path Posix (Rel r) File'
getJobExecutorTypesImportPath PgBoss = makeSdkImportPath [relfileP|jobs/pgBoss/types|]